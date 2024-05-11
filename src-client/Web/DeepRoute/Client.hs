{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}

module Web.DeepRoute.Client
    ( bodyReaderToLazyByteString
    , jsonBody
    , bytesBody
    , ClientEnv(..)
    , ClientError(..)
    , doRequestParsed
    , doRequestEither
    , doRequestThrow
    , doRequestUnchecked
    , defaultResponseChecks
    , (/@)
    , (/@@)

    , ApiRequest(..)
    , mkApiRequest
    , requestPath
    , requestQuery
    , requestAcceptable
    , requestHeaders
    , requestBody
    , requestMethod
    , requestModifier
    , requestResponseParser

    , checkAcceptable
    , checkSuccessful
    ) where

import Control.Exception (evaluate)
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Bitraversable
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Maybe
import Data.Text(Text)
import qualified Data.Text.Encoding as T
import System.IO.Unsafe(unsafeInterleaveIO)

import GHC.Generics

import qualified Network.HTTP.Client.Internal as Client
import Network.HTTP.Media
import Network.HTTP.Types
import Web.HttpApiData
import Network.HTTP.Client (Response(responseBody))
import Control.Exception.Safe
import Control.Monad.Except
import Data.Void (Void)
import Data.Traversable (for)
import Data.ByteString.Lazy (LazyByteString)

-- morally just converts a BodyReader into a lazy bytestring faithfully, i.e.
-- without buffering
bodyReaderToLazyByteString :: Client.BodyReader -> IO LBS.ByteString
bodyReaderToLazyByteString reader = do
    brLazy reader
    where
    brLazy br = unsafeInterleaveIO $ do
        next <- Client.brRead br
        if BS.null next then return mempty
        else LBS.chunk next <$> brLazy br

-- this isn't lazy really, but it does at least interleave response body
-- consumption with parsing.
jsonBody :: FromJSON a => LazyByteString -> IO (Either AesonException a)
jsonBody bytes =
    bitraverse evaluate evaluate $
        over _Left AesonException (eitherDecode' bytes)

bytesBody :: LazyByteString -> IO ByteString
bytesBody = evaluate . LBS.toStrict

data ClientEnv
    = ClientEnv
    { _clientEnvHost :: !ByteString
    , _clientEnvPort :: !Int
    , _clientEnvSecure :: !Bool
    , _clientEnvManager :: !Client.Manager
    , _clientEnvRequestModifier :: !(Client.Request -> Client.Request)
    }

data ClientError e
    = UnacceptableResponse !ByteString
    -- ^ the content type header's contents
    | UnsuccessfulStatus !Status
    | UserClientError e
    | ClientHTTPException !Client.HttpException
    deriving Show

instance (Show e, Typeable e) => Exception (ClientError e)

data ApiRequest a
    = ApiRequest
    { _requestPath :: BSB.Builder
    -- the user fills this out directly, using the toQueryParam function
    , _requestQuery :: !QueryText
    , _requestAcceptable :: !(Maybe [Quality MediaType])
    , _requestHeaders :: !RequestHeaders
    , _requestBody :: !Client.RequestBody
    , _requestMethod :: !Method
    , _requestModifier :: !(Client.Request -> Client.Request)
    , _requestResponseParser :: !(Client.Response LBS.ByteString -> IO (Client.Response a))
    }
    deriving (Functor)

makeLenses ''ApiRequest

makeClassy ''ClientEnv

mkApiRequest
    :: Method
    -> (Client.Response LBS.ByteString -> IO (Client.Response a))
    -> BSB.Builder
    -> ApiRequest a
mkApiRequest method parser path = ApiRequest
    { _requestPath = path
    , _requestMethod = method
    , _requestQuery = []
    , _requestAcceptable = Nothing
    , _requestHeaders = []
    , _requestBody = Client.RequestBodyBS mempty
    , _requestModifier = id
    , _requestResponseParser = parser
    }

debugPrintApiRequest :: ApiRequest a -> String
debugPrintApiRequest (ApiRequest p q a h _ m _ _) = unwords
    ["ApiRequest", show (BSB.toLazyByteString p), show q, show a, show h, show m]

doRequestParsed :: ClientEnv -> ApiRequest a -> (Client.Response a -> IO r) -> IO r
doRequestParsed env req kont = do
    let acceptHeader = maybe [] (\ts -> [("Accept", renderHeader ts)]) (_requestAcceptable req)
    let req' =
            Client.defaultRequest
                { Client.method = _requestMethod req
                , Client.secure = _clientEnvSecure env
                , Client.host = _clientEnvHost env
                , Client.port = _clientEnvPort env
                , Client.path = LBS.toStrict $ BSB.toLazyByteString $ _requestPath req
                , Client.queryString = LBS.toStrict $ BSB.toLazyByteString $ renderQueryText True (_requestQuery req)
                , Client.requestHeaders = acceptHeader <> _requestHeaders req
                , Client.requestBody = _requestBody req
                }
    let req'' = _requestModifier req (_clientEnvRequestModifier env req')
    Client.withResponse req'' (_clientEnvManager env) $ \resp -> do
        resp' <- traverse bodyReaderToLazyByteString resp
        kont =<<
            _requestResponseParser req resp'
{-# inlinable doRequestParsed #-}

checkAcceptable :: ApiRequest a -> Client.Response r -> ExceptT (ClientError e) IO ()
checkAcceptable req resp
    | Just acceptables <- _requestAcceptable req
    , Just responseContentTypeHeader <- lookup "Content-Type" (Client.responseHeaders resp) =
        case parseAccept responseContentTypeHeader of
            Just responseContentType
                | Just _ <- matchQuality [responseContentType] acceptables
                    -> return ()
            _ -> throwError (UnacceptableResponse responseContentTypeHeader)
    | otherwise = return ()

checkSuccessful :: Client.Response r -> ExceptT (ClientError e) IO ()
checkSuccessful resp
    | statusIsSuccessful (Client.responseStatus resp) = return ()
    | otherwise = throwError (UnsuccessfulStatus (Client.responseStatus resp))

defaultResponseChecks
    :: ApiRequest a -> Client.Response r -> ExceptT (ClientError e) IO ()
defaultResponseChecks req resp = do
    -- check success first, many servers don't want to give acceptable
    -- content types in error situations
    checkSuccessful resp
    checkAcceptable req resp
{-# inline defaultResponseChecks #-}

doRequestEither
    :: ClientEnv
    -> ApiRequest (Either e a)
    -> IO (Either (ClientError e) (Client.Response a))
doRequestEither env req = doRequestParsed env req $ \resp ->
    runExceptT $ do
        defaultResponseChecks req resp
        traverse (either (throwError . UserClientError) return) resp
{-# inlinable doRequestEither #-}

doRequestThrow
    :: (Typeable e, Show e)
    => ClientEnv
    -> ApiRequest (Either e a)
    -> IO (Client.Response a)
doRequestThrow env req =
    either throwIO return =<<
        doRequestEither env req
{-# inlinable doRequestThrow #-}

-- Careful! Status and returned content type both go unchecked.
doRequestUnchecked
    :: ClientEnv
    -> ApiRequest a
    -> IO (Client.Response ByteString)
doRequestUnchecked env req =
    doRequestParsed env req { _requestResponseParser = traverse bytesBody }  return
{-# inlinable doRequestUnchecked #-}

infixl 3 /@
(/@) :: BSB.Builder -> ByteString -> BSB.Builder
r /@ s = r <> BSB.char8 '/' <> BSB.byteString s
{-# inline conlike (/@) #-}

infixl 3 /@@
(/@@) :: ToHttpApiData a => BSB.Builder -> a -> BSB.Builder
r /@@ s = r <> BSB.char8 '/' <> toEncodedUrlPiece s
{-# inline conlike (/@@) #-}

{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Web.DeepRoute.Client where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson
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

readLazyResponseBody :: (LBS.ByteString -> IO r) -> Client.Response Client.BodyReader -> IO r
readLazyResponseBody kont resp = do
    bodyBytes <- brLazy (Client.responseBody resp)
    kont bodyBytes
    where
    brLazy br = unsafeInterleaveIO $ do
        next <- Client.brRead br
        if BS.null next then return mempty
        else LBS.chunk next <$> brLazy br

readJsonResponseBody :: FromJSON a => Client.Response Client.BodyReader -> IO (Maybe a)
readJsonResponseBody = readLazyResponseBody (evaluate . decode')

data ClientEnv
    = ClientEnv
    { _clientEnvHost :: !ByteString
    , _clientEnvPort :: !Int
    , _clientEnvSecure :: !Bool
    , _clientEnvManager :: !Client.Manager
    , _clientEnvRequestModifier :: !(Client.Request -> Client.Request)
    }

data ClientError
    = UnacceptableResponse !MediaType
    | UnsuccessfulStatus !Status
    deriving Show

instance Exception ClientError

makeClassy ''ClientEnv

data ApiRequest
    = ApiRequest
    { _requestPath :: BSB.Builder
    -- the user fills this out directly, using the toQueryParam function
    , _requestQuery :: !QueryText
    , _requestAcceptable :: !(Maybe [Quality MediaType])
    , _requestSuccessful :: !(Status -> Bool)
    , _requestHeaders :: !RequestHeaders
    , _requestBody :: !Client.RequestBody
    , _requestMethod :: !Method
    , _requestModifier :: !(Client.Request -> Client.Request)
    }

debugPrintApiRequest (ApiRequest p q a _ h _ m _) = unwords
    ["ApiRequest", show (BSB.toLazyByteString p), show q, show a, show h, show m]

makeLenses ''ApiRequest

doRequest :: ClientEnv -> ApiRequest -> (Client.Response Client.BodyReader -> IO a) -> IO a
doRequest env req kont = do
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
                , Client.checkResponse = \_ resp ->
                    let status = Client.responseStatus resp
                    in unless (_requestSuccessful req status) $ throwIO $ UnsuccessfulStatus status
                }
    let req'' = _requestModifier req (_clientEnvRequestModifier env req')
    Client.withResponse req'' (_clientEnvManager env) $ \resp -> do
        let responseContentTypeHeader = parseAccept =<< lookup "Content-Type" (Client.responseHeaders resp)
        case (_requestAcceptable req, responseContentTypeHeader) of
            (Just acceptables, Just responseContentType)
                -- here we do our own "content type negotiation" with the only option
                -- being the actually-returned content type, to determine if the response
                -- content type was acceptable.
                | Nothing <- matchQuality [responseContentType] acceptables ->
                    throwIO (UnacceptableResponse responseContentType)
            _ ->
                kont resp
{-# inlinable doRequest #-}

doRequestForEffect :: ClientEnv -> ApiRequest -> IO ()
doRequestForEffect env req = doRequest env req (const (return ()))
{-# inline conlike doRequestForEffect #-}

doJSONRequest :: FromJSON a => ClientEnv -> ApiRequest -> IO (Client.Response a)
doJSONRequest env req = do
    resp <- doJSONRequest' env req
    body' <- evaluate $ fromMaybe (error "invalid json in response") $ responseBody resp
    return resp { responseBody = body' }
{-# inlinable doJSONRequest #-}

doJSONRequest' :: FromJSON a => ClientEnv -> ApiRequest -> IO (Client.Response (Maybe a))
doJSONRequest' env req =
    doRequest env req $ \resp -> do
        json <- readJsonResponseBody resp
        return resp { responseBody = json }
{-# inline conlike doJSONRequest' #-}

infixl 2 `withMethod`
withMethod :: BSB.Builder -> Method -> ApiRequest
withMethod r m = ApiRequest
    { _requestPath = r
    , _requestQuery = []
    , _requestAcceptable = Nothing
    , _requestSuccessful = statusIsSuccessful
    , _requestHeaders = []
    , _requestBody = Client.RequestBodyBS mempty
    , _requestMethod = m
    , _requestModifier = id
    }
{-# inline conlike withMethod #-}

infixl 3 /@
(/@) :: BSB.Builder -> ByteString -> BSB.Builder
r /@ s = r <> BSB.char8 '/' <> BSB.byteString s
{-# inline conlike (/@) #-}

infixl 3 /@@
(/@@) :: ToHttpApiData a => BSB.Builder -> a -> BSB.Builder
r /@@ s = r <> BSB.char8 '/' <> toEncodedUrlPiece s
{-# inline conlike (/@@) #-}

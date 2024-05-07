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

readLazyResponseBody :: (LBS.ByteString -> IO r) -> Client.Response Client.BodyReader -> IO r
readLazyResponseBody kont resp = do
    bodyBytes <- brLazy (Client.responseBody resp)
    kont bodyBytes
    where
    brLazy br = unsafeInterleaveIO $ do
        next <- Client.brRead br
        if BS.null next then return mempty
        else LBS.chunk next <$> brLazy br

readJsonResponseBody :: FromJSON a => (Maybe a -> IO r) -> Client.Response Client.BodyReader -> IO r
readJsonResponseBody kont = readLazyResponseBody (kont . decode')

data ClientEnv
    = ClientEnv
    { _host :: !ByteString
    , _port :: !Int
    , _secure :: !Bool
    , _manager :: !Client.Manager
    , _responseTimeout :: !Client.ResponseTimeout
    }

data ClientError
    = UnacceptableResponse !MediaType
    | UnsuccessfulStatus !Status
    deriving Show

instance Exception ClientError

makeClassy ''ClientEnv

data ApiRequest
    = ApiRequest
    { _requestPath :: !BSB.Builder
    -- the user fills this out directly, using the toQueryParam function
    , _requestQuery :: !QueryText
    , _requestAcceptable :: !(Maybe [Quality MediaType])
    , _requestSuccessful :: !(Status -> Bool)
    , _requestHeaders :: !RequestHeaders
    , _requestBody :: !Client.RequestBody
    , _requestMethod :: !Method
    }

debugPrintApiRequest (ApiRequest p q _ _ h _ m) = unwords
    ["ApiRequest", show (BSB.toLazyByteString p), show q, show h, show m]

makeLenses ''ApiRequest

doRequest :: ClientEnv -> ApiRequest -> (Client.Response Client.BodyReader -> IO a) -> IO a
doRequest env req kont = do
    let acceptHeader = maybe [] (\ts -> [("Accept", renderHeader ts)]) (_requestAcceptable req)
    let req' =
            Client.defaultRequest
                { Client.method = _requestMethod req
                , Client.secure = _secure env
                , Client.host = _host env
                , Client.port = _port env
                , Client.path = LBS.toStrict $ BSB.toLazyByteString $ _requestPath req
                , Client.queryString = LBS.toStrict $ BSB.toLazyByteString $ renderQueryText True (_requestQuery req)
                , Client.requestHeaders = acceptHeader <> _requestHeaders req
                , Client.requestBody = _requestBody req
                , Client.checkResponse = \_ resp ->
                    let status = Client.responseStatus resp
                    in unless (_requestSuccessful req status) $ throwIO $ UnsuccessfulStatus status
                , Client.responseTimeout = _responseTimeout env
                }
    Client.withResponse req' (_manager env) $ \resp -> do
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

doRequestForEffect :: ClientEnv -> ApiRequest -> IO ()
doRequestForEffect env req = doRequest env req (const (return ()))

doJSONRequest :: FromJSON a => ClientEnv -> ApiRequest -> IO a
doJSONRequest env req =
    doJSONRequest' env req (evaluate . fromMaybe (error "invalid response body"))

doJSONRequest' :: FromJSON a => ClientEnv -> ApiRequest -> (Maybe a -> IO r) -> IO r
doJSONRequest' env req kont =
    doRequest env req (readJsonResponseBody kont)

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
    }

infixl 3 /@
(/@) :: BSB.Builder -> ByteString -> BSB.Builder
r /@ s = r <> BSB.char8 '/' <> BSB.byteString s

infixl 3 /@@
(/@@) :: ToHttpApiData a => BSB.Builder -> a -> BSB.Builder
r /@@ s = r <> BSB.char8 '/' <> toEncodedUrlPiece s

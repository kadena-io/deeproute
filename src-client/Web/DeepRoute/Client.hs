{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Web.DeepRoute.Client where

import Control.Exception
import Control.Lens
import Data.Aeson
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Text(Text)
import qualified Data.Text.Encoding as T
import System.IO.Unsafe(unsafeInterleaveIO)

import GHC.Generics

import qualified Network.HTTP.Client.Internal as Client
import Network.HTTP.Types
import Web.HttpApiData

readJsonResponseBody :: FromJSON a => (a -> IO r) -> IO r -> Client.Response Client.BodyReader -> IO r
readJsonResponseBody kont fallback resp = do
    bodyBytes <- brLazy (Client.responseBody resp)
    maybe fallback kont $ decode' bodyBytes
    where
    brLazy br = unsafeInterleaveIO $ do
        next <- Client.brRead br
        if BS.null next then return mempty
        else LBS.chunk next <$> brLazy br

data ClientEnv
    = ClientEnv
    { _host :: !ByteString
    , _port :: !Int
    , _secure :: !Bool
    , _manager :: !Client.Manager
    }

makeLenses ''ClientEnv

data ApiRequest
    = ApiRequest
    { _requestPath :: !BSB.Builder
    , _requestQuery :: !QueryText
    , _requestHeaders :: !RequestHeaders
    , _requestBody :: !Client.RequestBody
    , _requestMethod :: !Method
    }

makeLenses ''ApiRequest

doRequest :: ApiRequest -> ClientEnv -> (Client.Response Client.BodyReader -> IO a) -> IO a
doRequest req env kont = do
    let req' =
            Client.defaultRequest
                { Client.method = _requestMethod req
                , Client.secure = _secure env
                , Client.host = _host env
                , Client.port = _port env
                , Client.path = LBS.toStrict $ BSB.toLazyByteString $ _requestPath req
                , Client.queryString = LBS.toStrict $ BSB.toLazyByteString $ renderQueryText True (_requestQuery req)
                , Client.requestHeaders = _requestHeaders req
                , Client.requestBody = _requestBody req
                }
    Client.withResponse req' (_manager env) kont

doJSONRequest :: FromJSON a => ApiRequest -> ClientEnv -> (a -> IO r) -> IO r -> IO r
doJSONRequest req env kont fallback =
    doRequest req env (readJsonResponseBody kont fallback)

withMethod :: Method -> ApiRequest
withMethod m = ApiRequest
    { _requestPath = "/"
    , _requestQuery = []
    , _requestHeaders = []
    , _requestBody = Client.RequestBodyBS mempty
    , _requestMethod = m
    }

infixl 1 /
(/) :: ToHttpApiData a => ApiRequest -> a -> ApiRequest
r / s = r & requestPath <>~ ("/" <> toEncodedUrlPiece s)
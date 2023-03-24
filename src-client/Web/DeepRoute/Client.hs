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
import Network.HTTP.Types
import Web.HttpApiData

readJsonResponseBody :: FromJSON a => (Maybe a -> IO r) -> Client.Response Client.BodyReader -> IO r
readJsonResponseBody kont resp = do
    bodyBytes <- brLazy (Client.responseBody resp)
    kont $ decode' bodyBytes
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

makeClassy ''ClientEnv

class HasRouteRoot s where
    routeRoot :: Lens' s ByteString

data RootedClientEnv = RootedClientEnv
    { _rootedClientEnvRoot :: !ByteString
    , _rootedClientEnvEnv :: !ClientEnv
    }

makeLenses ''RootedClientEnv

instance HasClientEnv RootedClientEnv where
    clientEnv = rootedClientEnvEnv

instance HasRouteRoot RootedClientEnv where
    routeRoot = rootedClientEnvRoot

instance HasRouteRoot ByteString where
    routeRoot = id

data ApiRequest
    = ApiRequest
    { _requestPath :: !BSB.Builder
    , _requestQuery :: !QueryText
    , _requestHeaders :: !RequestHeaders
    , _requestBody :: !Client.RequestBody
    , _requestMethod :: !Method
    }

debugPrintApiRequest (ApiRequest p q h _ m) = unwords
    ["ApiRequest", show (BSB.toLazyByteString p), show q, show h, show m]

makeLenses ''ApiRequest

doRequest :: ClientEnv -> ApiRequest -> (Client.Response Client.BodyReader -> IO a) -> IO a
doRequest env req kont = do
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

doRequestForEffect :: ClientEnv -> ApiRequest -> IO ()
doRequestForEffect env req = doRequest env req (const (return ()))

doJSONRequest :: FromJSON a => ClientEnv -> ApiRequest -> IO a
doJSONRequest env req =
    doJSONRequest' env req (evaluate . fromMaybe (error "invalid response body"))

doJSONRequest' :: FromJSON a => ClientEnv -> ApiRequest -> (Maybe a -> IO r) -> IO r
doJSONRequest' env req kont =
    doRequest env req (readJsonResponseBody kont)

withMethod :: HasRouteRoot e => e -> Method -> ApiRequest
withMethod e m = ApiRequest
    { _requestPath = BSB.byteString (e ^. routeRoot)
    , _requestQuery = []
    , _requestHeaders = []
    , _requestBody = Client.RequestBodyBS mempty
    , _requestMethod = m
    }

infixl 1 /@
(/@) :: ApiRequest -> ByteString -> ApiRequest
r /@ s = r & requestPath <>~ (BSB.byteString $ "/" <> s)

infixl 1 //
(//) :: ToHttpApiData a => ApiRequest -> a -> ApiRequest
r // s = r & requestPath <>~ ("/" <> toEncodedUrlPiece s)

module Web.DeepRoute.Client where

import Control.Exception
import Data.Aeson
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Text(Text)
import qualified Data.Text.Encoding as T
import System.IO.Unsafe(unsafeInterleaveIO)

import GHC.Generics

import qualified Network.HTTP.Client.Internal as Client
import Network.HTTP.Types
import Web.HttpApiData

readJsonResponseBody :: FromJSON a => Client.Response Client.BodyReader -> IO (Maybe a)
readJsonResponseBody resp = do
    bodyBytes <- brLazy (Client.responseBody resp)
    return $! maybe Nothing (Just $!) $ decode' bodyBytes
    where
    brLazy br = unsafeInterleaveIO $ do
        next <- Client.brRead br
        if BS.null next then return mempty
        else LBS.chunk next <$> brLazy br

data ClientEnv
    = ClientEnv
    { host :: !ByteString
    , port :: !Int
    , secure :: !Bool
    , manager :: !Client.Manager
    }

data ApiRequest
    = ApiRequest
    { requestPath :: !ByteString
    , requestQuery :: !Query
    , requestHeaders :: !RequestHeaders
    , requestBody :: !Client.RequestBody
    , requestMethod :: !Method
    }

request :: ApiRequest -> ClientEnv -> (Client.Response Client.BodyReader -> IO a) -> IO a
request req env kont = do
    let req' =
            Client.defaultRequest
                { Client.method = requestMethod req
                , Client.secure = secure env
                , Client.host = host env
                , Client.port = port env
                , Client.path = requestPath req
                , Client.queryString = renderQuery True $ requestQuery req
                , Client.requestHeaders = requestHeaders req
                , Client.requestBody = requestBody req
                }
    Client.withResponse req' (manager env) kont

requestJSON :: FromJSON a => ApiRequest -> ClientEnv -> IO (Maybe a)
requestJSON req env =
    request req env readJsonResponseBody
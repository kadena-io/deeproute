{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DerivingStrategies #-}

module Web.DeepRoute.Wai
    ( responseJSON
    , requestFromJSON
    , routeWaiApp
    , jsonApp
    , getParams
    ) where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.CaseInsensitive(CI)
import qualified Data.CaseInsensitive as CI
import Data.Function
import Data.Functor
import Data.Functor.Identity
import qualified Data.HashSet as HS
import Data.List
import Data.Maybe
import Data.Text(Text)
import qualified Data.Text.Encoding as T
import GHC.Magic

import Network.HTTP.Types
import Network.HTTP.Media
import qualified Network.Wai as Wai
import Web.HttpApiData

import Web.DeepRoute

responseJSON :: ToJSON a => Status -> ResponseHeaders -> a -> Wai.Response
responseJSON s rh = Wai.responseLBS s rh . encode

requestFromJSON :: FromJSON a => Wai.Request -> IO a
requestFromJSON req =
    decode' <$> Wai.lazyRequestBody req >>= \case
        Just !decodedBody -> do
            return decodedBody
        Nothing ->
            errorWithStatus badRequest400 "invalid request body"

routeWaiApp
    :: Wai.Request
    -> (Wai.Response -> IO Wai.ResponseReceived)
    -> IO Wai.ResponseReceived
    -> Route Wai.Application
    -> IO Wai.ResponseReceived
routeWaiApp req resp fallback tree =
    handle (earlyExit resp) $
        runRoute tree lose win (Wai.requestMethod req) acceptHeader (HeadAlteration hd) (OptionsResponse opts) (Wai.pathInfo req)
    where
    hd ct app = app req (emptyResponseBody resp)
    emptyResponseBody respond response =
        respond $ Wai.responseLBS (Wai.responseStatus response) (Wai.responseHeaders response) ""
    opts allowedMethods =
        resp $ Wai.responseLBS noContent204 [("Allow", BS.intercalate ", " allowedMethods)] ""
    acceptHeader = AcceptHeader <$> lookup "Accept" (Wai.requestHeaders req)
    lose (InvalidUrlPathPiece t) =
        errorWithStatus badRequest400 $ "invalid url path piece: " <> t
    lose RouteNotFound =
        fallback
    lose WrongMethod =
        errorWithStatus methodNotAllowed405 ""
    lose NotAcceptable =
        errorWithStatus notAcceptable406 ""
    win ct app =
        app req (resp . setContentType ct)
    -- if there is no response body, do not set the content type
    setContentType Nothing =
        id
    setContentType (Just mt) =
        -- this only works if we don't use the "raw" Wai response type.
        Wai.mapResponseHeaders (\hs -> cth : [h | h@(n,_) <- hs, n /= "Content-Type"])
        where
        cth = ("Content-Type", renderHeader mt)
    earlyExit resp (HTTPEarlyExitException status mt body) =
        resp $ Wai.responseLBS
            status
            [("Content-Type", renderHeader mt)]
            (LBS.fromStrict $
                if mt == "text/plain;charset=utf-8" && BS.null body
                then statusMessage status
                else body
            )
{-# inline routeWaiApp #-}

jsonApp :: (FromJSON a, ToJSON b) => (a -> IO b) -> Wai.Application
jsonApp k req resp =
    resp . responseJSON status200 [("Content-Type", "application/json")]
        =<< k
        =<< requestFromJSON req

getParams :: Wai.Request -> QueryParamParser a -> IO a
getParams req (QueryParamParser parser) = runReaderT parser query
    where
    query = queryToQueryText $ Wai.queryString req

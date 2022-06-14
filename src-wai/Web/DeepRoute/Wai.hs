{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Web.DeepRoute.Wai
    ( responseJSON
    , requestFromJSON
    , routeWaiApp
    , jsonApp
    , getParams
    ) where

import Control.Exception
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
        Just !decodedBody ->
            return decodedBody
        Nothing ->
            throwIO $ HTTPEarlyExitException badRequest400 (Just "invalid request body")

routeWaiApp :: Route Wai.Application -> Wai.Application
routeWaiApp tree req resp =
    handle earlyExit $ app req (resp . setContentType ct)
    where
    acceptHeader = AcceptHeader <$> lookup "Accept" (Wai.requestHeaders req)
    (ct, app) =
        runRoute tree lose (,) (Wai.requestMethod req) acceptHeader (Wai.pathInfo req)
    lose InvalidUrlPathPiece =
        errorWithStatus badRequest400 (Just "invalid url path piece")
    lose RouteNotFound =
        errorWithStatus notFound404 Nothing
    lose NothingToCapture =
        errorWithStatus notFound404 (Just "required url path piece was absent")
    lose WrongMethod =
        errorWithStatus methodNotAllowed405 Nothing
    lose NotAcceptable =
        errorWithStatus notAcceptable406 Nothing
    earlyExit (HTTPEarlyExitException status body) =
        resp $ Wai.responseLBS
            status
            [("Content-Type", "text/plain;charset=utf-8")]
            (LBS.fromStrict $ fromMaybe (statusMessage status) body)
    setContentType mt =
        -- this only works if we don't use the "raw" Wai response type.
        Wai.mapResponseHeaders (\hs -> cth : [h | h@(n,_) <- hs, n /= "Content-Type"])
        where
        cth = ("Content-Type", renderHeader mt)
{-# inline routeWaiApp #-}

jsonApp :: (FromJSON a, ToJSON b) => (a -> IO b) -> Wai.Application
jsonApp k req resp =
    resp . responseJSON status200 [("Content-Type", "application/json")]
        =<< k
        =<< requestFromJSON req

getParams :: Wai.Request -> (QueryText -> a) -> a
getParams req parser = parser query
    where
    query = queryToQueryText $ Wai.queryString req

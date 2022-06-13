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
import Data.Map(Map)
import qualified Data.Map as Map
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

newtype MT = MT MediaType
    deriving newtype Show

instance Accept MT where
    parseAccept = fmap MT . parseAccept
    matches (MT a) (MT b)
        | mainType b == "*" = params
        | subType b == "*"  = mainType a == mainType b && params
        | otherwise         = (main && sub && params)
      where
        normalizedParameters mt
            | mainType mt == "application"
            , subType mt == "json"
                = Map.alter (removeCharsetUtf8 mt) "charset" (parameters mt)
            | otherwise = parameters mt
        removeCharsetUtf8 mt (Just "utf-8") = Nothing
        removeCharsetUtf8 mt p = p
        main = mainType a == mainType b
        sub = subType a == subType b
        params = Map.null (normalizedParameters b) || normalizedParameters a == normalizedParameters b

    moreSpecificThan (MT a) (MT b) = (a `matches` b &&) $
        mainType a == "*" && anyB && params ||
        subType a == "*" && (anyB || subB && params) ||
        anyB || subB || params
      where
        anyB = mainType b == "*"
        subB = subType b == "*"
        params = not (Map.null $ parameters a) && Map.null (parameters b)

routeWaiApp :: Route [(MediaType, Wai.Application)] -> Wai.Application
routeWaiApp tree req resp = do
    let
        apps = (\(mt, app) -> (mt, (mt, app))) <$>
            runRoute tree lose id (Wai.requestMethod req) (Wai.pathInfo req)
        acceptHeader = lookup "Accept" (Wai.requestHeaders req)
    handle earlyExit $ case acceptHeader of
        Just accept -> case mapAccept apps =<< parseAccept accept of
            Just (ct, app) -> app req (setContentType ct resp)
            Nothing -> errorWithStatus notAcceptable406 Nothing
        Nothing -> case apps of
            -- with no accept header, we always return the first content type
            (_,(ct,app)):_ -> app req (setContentType ct resp)
            [] -> error "there is no way to respond"
    where
    lose InvalidUrlPathPiece =
        errorWithStatus badRequest400 (Just "invalid url path piece")
    lose RouteNotFound =
        errorWithStatus notFound404 Nothing
    lose NothingToCapture =
        errorWithStatus notFound404 (Just "required url path piece was absent")
    lose WrongMethod =
        errorWithStatus methodNotAllowed405 Nothing
    earlyExit (HTTPEarlyExitException status body) =
        resp $ Wai.responseLBS
            status
            [("Content-Type", "text/plain;charset=utf-8")]
            (LBS.fromStrict $ fromMaybe (statusMessage status) body)
    setContentType mt resp =
        -- this only works if we don't use the "raw" Wai response type.
        resp . Wai.mapResponseHeaders (\hs -> cth : [h | h@(n,_) <- hs, n /= "Content-Type"])
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

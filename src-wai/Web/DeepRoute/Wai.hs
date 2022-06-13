{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Web.DeepRoute.Wai where

import Control.Exception
-- import Control.Lens
-- import Control.Monad.Catch hiding (bracket)
import Control.Monad.Reader as Reader
import Data.Aeson
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.CaseInsensitive(CI)
import qualified Data.CaseInsensitive as CI
import Data.DList(DList)
import qualified Data.DList as DList
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
import Data.Functor.Compose
import System.IO.Unsafe

import GHC.Generics

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

routeWaiApp :: Route [(MediaType, Wai.Application)] -> Wai.Application
routeWaiApp tree req resp = do
    let
        apps = (\(mt, app) -> (mt, (mt, app))) <$>
            runRoute tree lose id (Wai.requestMethod req) (Wai.pathInfo req)
        acceptHeader = lookup "Accept" (Wai.requestHeaders req)
    handle earlyExit $ case acceptHeader of
        Just accept -> case parseAccept =<< mapAccept apps accept of
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

allParams :: Wai.Request -> (QueryText -> a) -> a
allParams req parser = parser query
    where
    query = queryToQueryText $ Wai.queryString req

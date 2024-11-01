
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module: Web.DeepRoute
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- Utilities used to write HTTP servers.
--

module Web.DeepRoute
    ( HTTPEarlyExitException(..)
    , jsonErrorWithStatus
    , errorWithStatus
    , RoutingError(..)
    , Route(..)
    , HeadAlteration(..)
    , OptionsResponse(..)
    , AcceptHeader(..)
    , seg
    , capture
    , capture'
    , noMoreChoices
    , jsonEndpoint
    , endpoint
    , multiEndpoint
    , queryParam
    , queryParamMaybe
    , queryParamOptional
    , QueryParamParser(..)
    , QueryParam(..)
    )
    where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Coerce
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text.Encoding as T

import GHC.Generics

import Network.HTTP.Media
import Network.HTTP.Types
import Web.HttpApiData
import Data.Maybe
import GHC.Exts (noinline)

-- -------------------------------------------------------------------------- --
-- ** HTTP utilities

data HTTPEarlyExitException
    = HTTPEarlyExitException !Status !MediaType !ByteString
    deriving stock (Show)
    deriving anyclass (Exception)

jsonErrorWithStatus :: ToJSON e => Status -> e -> IO a
jsonErrorWithStatus s e =
    throwIO $ HTTPEarlyExitException s "application/json" (LBS.toStrict $ encode e)

errorWithStatus :: Status -> Text -> IO a
errorWithStatus s b =
    throwIO $ HTTPEarlyExitException s "text/plain" (T.encodeUtf8 b)

data RoutingError
    = RouteNotFound
    | WrongMethod
    | InvalidUrlPathPiece Text
    | NotAcceptable

newtype AcceptHeader = AcceptHeader ByteString

newtype HeadAlteration a r = HeadAlteration (Maybe MediaType -> a -> r)
newtype OptionsResponse r = OptionsResponse ([Method] -> r)

newtype Route a = Route
    { runRoute
        :: forall r. (RoutingError -> r) -> (Maybe MediaType -> a -> r)
        -> Method -> Maybe AcceptHeader -> HeadAlteration a r -> OptionsResponse r -> [Text]
        -> r
    }

instance Functor Route where
    fmap f rt = Route $ \lose win meth accept (HeadAlteration hd) opts path ->
        runRoute rt lose (\rmt -> win rmt . f) meth accept (HeadAlteration $ \rmt -> hd rmt . f) opts path
    {-# inline fmap #-}

instance Monoid (Route a) where
    mempty = noMoreChoices

instance Semigroup (Route a) where
    f <> s = Route $ \lose win meth accept hd opts path ->
        let
            fallback RouteNotFound = runRoute s lose win meth accept hd opts path
            fallback err = lose err
        in runRoute f fallback win meth accept hd opts path
    {-# INLINE (<>) #-}

-- | Require the next segment of the route to be equal to a fixed string.
seg :: Text -> Route a -> Route a
seg thisEle thisRoute = Route $ \lose win meth accept hd opts path ->
    case path of
        ele : rest | ele == thisEle ->
            runRoute thisRoute lose win meth accept hd opts rest
        _ -> lose RouteNotFound
{-# inline seg #-}

-- | Capture and parse the next segment of the route, using the
-- 'FromHttpApiData' type class to parse it.
capture :: forall e a. FromHttpApiData e => Route (e -> a) -> Route a
capture inner = capture' parseUrlPieceMaybe inner
{-# inline capture #-}

-- | Capture and parse the next segment of the route.
capture' :: (Text -> Maybe e) -> Route (e -> a) -> Route a
capture' parse inner = Route $ \lose win meth accept hd opts path ->
    case path of
        ele : rest -> case parse ele of
            Just cap -> runRoute (($ cap) <$> inner) lose win meth accept hd opts rest
            Nothing -> lose (InvalidUrlPathPiece ele)
        _ -> lose RouteNotFound
{-# inline capture' #-}

-- | mempty for route concatenation, the empty route, 404.
noMoreChoices :: Route a
noMoreChoices = Route $ \lose _ _ _ _ _ _ -> lose RouteNotFound
{-# inline conlike noMoreChoices #-}

-- | A single endpoint with a single method, returning a JSON Response body.
-- Note that concatenating `endpoint` routes does not allow having multiple
-- routes at the same path.
jsonEndpoint :: Method -> a -> Route a
jsonEndpoint m a = endpoint m (Just "application/json") a
{-# inline jsonEndpoint #-}

-- | A single endpoint with a single method and response body media type. Note
-- that concatenating `endpoint` routes does not allow having multiple routes
-- at the same path.
endpoint :: Method -> Maybe MediaType -> a -> Route a
endpoint m mt a = multiEndpoint [(m, mt, a)]
{-# inline endpoint #-}

-- | An endpoint with multiple implementations for different methods and
-- desired response body media types.
multiEndpoint :: forall a. [(Method, Maybe MediaType, a)] -> Route a
multiEndpoint xs = Route $ \lose win meth maybeAccept (HeadAlteration hd) (OptionsResponse opts) path ->
    let
        route :: forall p. Method -> p -> p -> (Maybe MediaType -> a -> p) -> p
        route m notAcceptable wrongMethod w
            -- if there are no ways to handle the request method
            | [] <- rightMethod = wrongMethod
            -- if there is a way to handle the request method, and there is no
            -- Accept header in the request, act as if the specified media type
            -- was the first media type
            | Nothing <- maybeAccept, ((_,(mt, a)):_) <- rightMethod = w (Just mt) a
            -- if there is no Accept header, and there is a way to respond with
            -- no response body, do that
            | Nothing <- acceptable, Just (rm, ra) <- noResponseRightMethod = w Nothing ra
            -- otherwise, if there is no way to respond in accordance with the
            -- client's acceptable response body types, say that
            | Nothing <- acceptable = notAcceptable
            -- otherwise, respond however is most acceptable
            | Just (mt,a) <- acceptable = w (Just mt) a
            where
            rightMethod = [ (MT rmt, (rmt, ra)) | (rm, Just rmt, ra) <- xs, rm == m ]
            acceptable =
                mapAccept rightMethod =<< coerce maybeAccept
            noResponseRightMethod =
                listToMaybe [ (rm, ra) | (rm, Nothing, ra) <- xs, rm == m ]
    in case path of
        -- the path must have no more components to match successfully
        []
            | meth == methodOptions ->
                opts [ mt | (mt, _, _) <- xs ]
            | meth == methodHead ->
                route methodHead (lose NotAcceptable) (route methodGet (lose NotAcceptable) (lose WrongMethod) hd) win
            | otherwise ->
                route meth (lose NotAcceptable) (lose WrongMethod) win
        -- the path has excess components that haven't been captured or
        -- matched against, 404
        _ -> lose RouteNotFound
{-# inlinable multiEndpoint #-}

-- | Query parameters may be present with a value or present without one.
data QueryParam a
    = QueryParamNoValue
    | QueryParamValue a

-- | A query parameter parser function.
newtype QueryParamParser a = QueryParamParser (ReaderT QueryText IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO)

queryParamOptional :: FromHttpApiData a => Text -> QueryParamParser (Maybe (QueryParam a))
queryParamOptional paramName = QueryParamParser $ ReaderT $ \q ->
    case (traverse.traverse) parseQueryParam $ lookup paramName q of
        Left _ -> liftIO $ errorWithStatus badRequest400 $
            "query parameter " <> paramName <> " has malformed value"
        Right Nothing -> return Nothing
        Right (Just Nothing) -> return $ Just QueryParamNoValue
        Right (Just (Just v)) -> return $ Just $ QueryParamValue v

queryParamMaybe :: FromHttpApiData a => Text -> QueryParamParser (Maybe a)
queryParamMaybe paramName =
    collapseNoValue <$> queryParamOptional paramName
    where
    collapseNoValue (Just QueryParamNoValue) = Nothing
    collapseNoValue (Just (QueryParamValue v)) = Just v
    collapseNoValue Nothing = Nothing

queryParam :: FromHttpApiData a => Text -> QueryParamParser a
queryParam paramName =
    mandatory =<< queryParamOptional paramName
    where
    mandatory Nothing = liftIO $ errorWithStatus badRequest400 $
        "mandatory query parameter " <> paramName <> " not included in URL"
    mandatory (Just QueryParamNoValue) = liftIO $ errorWithStatus badRequest400 $
        "mandatory query parameter " <> paramName <> " included in URL but has no value"
    mandatory (Just (QueryParamValue a)) = return a

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
        params = Map.null (normalizedParameters b)
            || normalizedParameters a == normalizedParameters b
            || parameters a == parameters b

    moreSpecificThan (MT a) (MT b) = (a `matches` b &&) $
        mainType a == "*" && anyB && params ||
        subType a == "*" && (anyB || subB && params) ||
        anyB || subB || params
      where
        anyB = mainType b == "*"
        subB = subType b == "*"
        params = not (Map.null $ parameters a) && Map.null (parameters b)

    hasExtensionParameters _ = True

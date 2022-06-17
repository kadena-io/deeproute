
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Web.DeepRoute
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- Utilities used to write HTTP servers.
--

module Web.DeepRoute where

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

-- -------------------------------------------------------------------------- --
-- ** HTTP utilities

data HTTPEarlyExitException
    = HTTPEarlyExitException !Status !MediaType !ByteString
    deriving stock (Show)
    deriving anyclass (Exception)

jsonErrorWithStatus :: ToJSON e => Status -> e -> IO a
jsonErrorWithStatus s e =
    throwIO $ HTTPEarlyExitException s "application/json" (LBS.toStrict $ encode e)

errorWithStatus :: Status -> ByteString -> IO a
errorWithStatus s b =
    throwIO $ HTTPEarlyExitException s "text/plain" b

data RoutingError
    = RouteNotFound
    | WrongMethod
    | InvalidUrlPathPiece Text
    | NotAcceptable

newtype AcceptHeader = AcceptHeader ByteString

newtype Route a = Route
    { runRoute
        :: forall r. (RoutingError -> r) -> (MediaType -> a -> r)
        -> Method -> Maybe AcceptHeader -> [Text]
        -> r
    }

instance Functor Route where
    fmap f rt = Route $ \lose win meth accept path ->
        runRoute rt lose (\rmt a -> win rmt (f a)) meth accept path
    {-# inline fmap #-}

instance Monoid (Route a) where
    mempty = noMoreChoices

instance Semigroup (Route a) where
    f <> s = Route $ \lose win meth accept path ->
        let
            fallback RouteNotFound = runRoute s lose win meth accept path
            fallback err = lose err
        in runRoute f fallback win meth accept path
    {-# INLINE (<>) #-}

choice :: Text -> Route a -> Route a
choice thisEle thisRoute = Route $ \lose win meth accept path ->
    case path of
        ele : rest | ele == thisEle ->
            runRoute thisRoute lose win meth accept rest
        _ -> lose RouteNotFound
{-# inline choice #-}

capture :: forall e a. FromHttpApiData e => Route (e -> a) -> Route a
capture inner = capture' parseUrlPieceMaybe inner

capture' :: (Text -> Maybe e) -> Route (e -> a) -> Route a
capture' parse inner = Route $ \lose win meth accept path ->
    case path of
        ele : rest -> case parse ele of
            Just !cap -> runRoute (($ cap) <$> inner) lose win meth accept rest
            Nothing -> lose (InvalidUrlPathPiece ele)
        _ -> lose RouteNotFound
{-# inline capture' #-}

noMoreChoices :: Route a
noMoreChoices = Route $ \lose _ _ _ _ -> lose RouteNotFound
{-# inline noMoreChoices #-}

terminus :: Method -> MediaType -> a -> Route a
terminus m mt a = terminus' [(m, mt, a)]

terminus' :: forall a. [(Method, MediaType, a)] -> Route a
terminus' xs = Route $ \lose win meth maybeAccept path -> let
    rightMethod = [ (MT rmt, (rm, rmt, ra)) | (rm, rmt, ra) <- xs, rm == meth ]
    acceptable = mapAccept rightMethod =<< coerce maybeAccept
    in case path of
        []
            | [] <- rightMethod -> lose WrongMethod
            | Nothing <- maybeAccept, ((_,(_,mt,a)):_) <- rightMethod -> win mt a
            | Nothing <- acceptable -> lose NotAcceptable
            | Just (_,mt,a) <- acceptable -> win mt a
        _ -> lose RouteNotFound
        where
{-# inline terminus #-}

data QueryParam a
    = QueryParamNoValue
    | QueryParamValue !a

newtype QueryParser a = QueryParser (ReaderT QueryText IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO)

queryParamOptional :: FromHttpApiData a => Text -> QueryParser (Maybe (QueryParam a))
queryParamOptional paramName = QueryParser $ ReaderT $ \q ->
    case (traverse.traverse) parseQueryParam $ lookup paramName q of
        Left _ -> liftIO $ errorWithStatus badRequest400 $
            "query parameter " <> T.encodeUtf8 paramName <> " has malformed value"
        Right Nothing -> return Nothing
        Right (Just Nothing) -> return $ Just QueryParamNoValue
        Right (Just (Just v)) -> return $ Just $! QueryParamValue v

queryParamMaybe :: FromHttpApiData a => Text -> QueryParser (Maybe a)
queryParamMaybe paramName =
    collapseNoValue <$> queryParamOptional paramName
    where
    collapseNoValue (Just QueryParamNoValue) = Nothing
    collapseNoValue (Just (QueryParamValue v)) = Just $! v
    collapseNoValue Nothing = Nothing

queryParam :: FromHttpApiData a => Text -> QueryParser a
queryParam paramName =
    mandatory =<< queryParamOptional paramName
    where
    mandatory Nothing = liftIO $ errorWithStatus badRequest400 $
        "mandatory query parameter " <> T.encodeUtf8 paramName <> " not included in URL"
    mandatory (Just QueryParamNoValue) = liftIO $ errorWithStatus badRequest400 $
        "mandatory query parameter " <> T.encodeUtf8 paramName <> " included in URL but has no value"
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
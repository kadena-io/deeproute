
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

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
-- import Control.Lens
-- import Control.Monad.Catch hiding (bracket)
import Control.Monad.Reader as Reader
import Data.Aeson
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.DList(DList)
import qualified Data.DList as DList
import qualified Data.HashSet as HS
import Data.Text(Text)
import qualified Data.Text.Encoding as T
import Data.Functor.Compose
import System.IO.Unsafe

import GHC.Generics

import Network.HTTP.Types
import Web.HttpApiData

-- -------------------------------------------------------------------------- --
-- ** HTTP utilities

data HTTPEarlyExitException
    = HTTPEarlyExitException !Status !(Maybe ByteString)
    deriving stock (Generic, Show)
    deriving anyclass (Exception)

errorWithStatus :: Status -> Maybe ByteString -> a
errorWithStatus s b =
    throw $ HTTPEarlyExitException s b

data RoutingError
    = RouteNotFound
    | NothingToCapture
    | WrongMethod
    | InvalidUrlPathPiece

newtype Route a = Route
    { runRoute
        :: forall r. (RoutingError -> r) -> (a -> r)
        -> Method -> [Text]
        -> r
    }

instance Functor Route where
    fmap f rt = Route $ \lose win meth path ->
        runRoute rt lose (win . f) meth path
    {-# inline fmap #-}

instance Monoid (Route a) where
    mempty = noMoreChoices

instance Semigroup (Route a) where
    f <> s = Route $ \lose win meth path ->
        let
            fallback RouteNotFound = runRoute s lose win meth path
            fallback err = lose err
        in runRoute f fallback win meth path
    {-# INLINE (<>) #-}

choice :: Text -> Route a -> Route a
choice thisEle thisRoute = Route $ \lose win meth path ->
    case path of
        ele : rest | ele == thisEle ->
            runRoute thisRoute lose win meth rest
        _ -> lose RouteNotFound
{-# inline choice #-}

capture :: forall e a. FromHttpApiData e => Route (e -> a) -> Route a
capture inner = capture' parseUrlPieceMaybe inner

capture' :: (Text -> Maybe e) -> Route (e -> a) -> Route a
capture' parse inner = Route $ \lose win meth path ->
    case path of
        ele : rest -> case parse ele of
            Just !cap -> runRoute (($ cap) <$> inner) lose win meth rest
            Nothing -> lose InvalidUrlPathPiece
        _ -> lose NothingToCapture
{-# inline capture' #-}

noMoreChoices :: Route a
noMoreChoices = Route $ \lose _ _ _ -> lose RouteNotFound
{-# inline noMoreChoices #-}

terminus :: [Method] -> a -> Route a
terminus allowedMethods a = Route $ \lose win meth path -> case path of
    [] | elem meth allowedMethods -> win a
       | otherwise -> lose WrongMethod
    _ -> lose RouteNotFound
{-# inline terminus #-}

data QueryParam a
    = QueryParamNoValue
    | QueryParamValue !a

queryParamOptional :: FromHttpApiData a => Text -> QueryText -> Maybe (QueryParam a)
queryParamOptional paramName q =
    case (traverse.traverse) parseQueryParam $ lookup paramName q of
        Left _ -> errorWithStatus badRequest400 $
            Just $ "query parameter " <> T.encodeUtf8 paramName <> " has malformed value"
        Right Nothing -> Nothing
        Right (Just Nothing) -> Just QueryParamNoValue
        Right (Just (Just v)) -> Just $ QueryParamValue v

queryParamMaybe :: FromHttpApiData a => Text -> QueryText -> Maybe a
queryParamMaybe paramName =
    collapseNoValue <$> queryParamOptional paramName
    where
    collapseNoValue (Just QueryParamNoValue) = Nothing
    collapseNoValue (Just (QueryParamValue v)) = Just v
    collapseNoValue Nothing = Nothing

queryParam :: FromHttpApiData a => Text -> QueryText -> a
queryParam paramName =
    mandatory <$> queryParamOptional paramName
    where
    mandatory Nothing = errorWithStatus badRequest400 $
        Just $ "mandatory query parameter " <> T.encodeUtf8 paramName <> " not included in URL"
    mandatory (Just QueryParamNoValue) = errorWithStatus badRequest400 $
        Just $ "mandatory query parameter " <> T.encodeUtf8 paramName <> " included in URL but has no value"
    mandatory (Just (QueryParamValue a)) = a

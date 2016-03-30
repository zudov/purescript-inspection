{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Refined.Extended
  ( module Refined
  ) where

import Refined

import Data.Text as Text
import Data.Bifunctor (first)
import Data.String (IsString(..))
import Data.Aeson.Extra (FromJSON(..), ToJSON(..), ToJSONKey(..))
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Data.SafeCopy
import Data.Typeable

instance (Predicate p x, IsString x) => IsString (Refined p x) where
  fromString = either error id . refine . fromString 

instance (Predicate p x, FromJSON x) => FromJSON (Refined p x) where
  parseJSON value = either fail pure . refine =<< parseJSON value

instance (Predicate p x, ToJSON x) => ToJSON (Refined p x) where
  toJSON = toJSON . unrefine

instance (Predicate p x, ToJSONKey x) => ToJSONKey (Refined p x) where
  toJSONKey = toJSONKey . unrefine

instance (Predicate p x, ToHttpApiData x) => ToHttpApiData (Refined p x) where
  toUrlPiece   = toUrlPiece . unrefine
  toHeader     = toHeader . unrefine
  toQueryParam = toQueryParam . unrefine

instance (Predicate p x, FromHttpApiData x) => FromHttpApiData (Refined p x) where
  parseUrlPiece   value = first Text.pack . refine =<< parseUrlPiece value
  parseHeader     value = first Text.pack . refine =<< parseHeader value
  parseQueryParam value = first Text.pack . refine =<< parseQueryParam value

-- TODO: Come up with something that would support versioning
instance (Predicate p x, SafeCopy x, Typeable p, Typeable x) => SafeCopy (Refined p x) where
  putCopy = contain . safePut . unrefine
  getCopy = contain (either fail pure . refine =<< safeGet)
  errorTypeName = show . typeRep

module MyLittlePrelude
  ( module Prelude
  , module Control.Category
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Control.Monad.Reader.Class
  , module Data.ByteString
  , module Data.Maybe
  , module Data.Hashable
  , module Data.Monoid
  , module Data.Text
  , module Data.Foldable
  , module Data.Typeable
  , module Data.Data
  , module Data.Vector
  , module Data.Map
  , module Data.HashMap.Strict
  , module Data.Set
  , module Data.Proxy
  , module Data.Time.Clock
  , module Data.HashSet
  , module GHC.Generics
  ) where

import Prelude hiding ((.), id)

import Control.Category (Category, id, (.), (<<<), (>>>))

import Control.Monad (when, unless, (>=>), (<=<), forM, forM_, mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)

import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, catMaybes)
import Data.Data (Data)
import Data.Monoid (Monoid, (<>), mempty)
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import Data.HashSet (HashSet)

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Foldable (find)

import GHC.Generics (Generic)

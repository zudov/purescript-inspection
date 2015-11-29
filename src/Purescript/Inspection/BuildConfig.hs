{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Purescript.Inspection.BuildConfig where

import Control.Monad
import Data.Aeson.Extra
import Data.Aeson.TH
import Data.Monoid
import Data.Typeable (Typeable())
import Data.Data (Data())
import Data.Map (Map)
import Data.SafeCopy
import qualified Data.Text as Text
import GHC.Generics (Generic())
import Servant.Common.Text

import Purescript.Inspection.ReleaseTag

data BuildConfig
  = BuildConfig { compiler        :: Compiler
                , compilerRelease :: ReleaseTag
                }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Compiler = Purescript
              deriving (Eq, Show, Ord, Generic, Typeable, Data)

instance ToText BuildConfig where
  toText BuildConfig{..} = toText compiler <> "-" <> toText compilerRelease

instance FromText BuildConfig where
  fromText (Text.splitOn "-" -> [compiler, compilerRelease]) =
      BuildConfig <$> fromText compiler
                  <*> fromText compilerRelease
  fromText _ = Nothing

instance ToJSONKey BuildConfig where
  toJSONKey = toText

instance ToJSON a => ToJSON (Map BuildConfig a) where
  toJSON = toJSON . M

instance ToText Compiler where
  toText Purescript = "purescript"

instance FromText Compiler where
  fromText "purescript" = Just Purescript
  fromText _ = Nothing

deriveSafeCopy 0 'base ''Compiler
deriveSafeCopy 0 'base ''BuildConfig

deriveToJSON defaultOptions ''BuildConfig

instance FromJSON Compiler where
  parseJSON (String s) = maybe mzero pure (fromText s)
  parseJSON _ = fail "'Compiler' should be a String"

instance ToJSON Compiler where
  toJSON = toJSON . toText

compilerRepo :: Compiler -> GithubLocation
compilerRepo Purescript = GithubLocation (GithubOwner "purescript") (GithubRepo "purescript")

getBuildConfigs :: Compiler -> IO [BuildConfig]
getBuildConfigs c = map (BuildConfig c) <$> getReleaseTags (compilerRepo c)

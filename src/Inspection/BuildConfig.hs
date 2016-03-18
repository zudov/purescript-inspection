{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Inspection.BuildConfig where

import           Control.Monad
import           Data.Data     (Data ())
import           Data.Map      (Map)
import           Data.Monoid
import qualified Data.Text     as Text
import           Data.Typeable (Typeable ())
import           GHC.Generics  (Generic ())

import Data.Aeson.Extra
import Data.Aeson.Types    (Options (..), defaultOptions)
import Data.SafeCopy       (base, deriveSafeCopy)
import Network.HTTP.Client (Manager)
import Servant.Common.Text (FromText (..), ToText (..))

import Inspection.PackageName
import Inspection.AuthToken
import Inspection.ReleaseTag

data BuildConfig
  = BuildConfig { buildConfigCompiler        :: Compiler
                , buildConfigCompilerRelease :: ReleaseTag
                }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Compiler = Purescript
              deriving (Eq, Show, Ord, Generic, Typeable, Data)

instance ToText BuildConfig where
  toText BuildConfig{..} = toText buildConfigCompiler
                        <> "-"
                        <> toText buildConfigCompilerRelease

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

instance ToJSON BuildConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modifier }
    where
      modifier "buildConfigCompiler" = "compiler"
      modifier "buildConfigCompilerRelease" = "compilerRelease"
      modifier a = a

instance FromJSON BuildConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modifier }
    where
      modifier "buildConfigCompiler" = "compiler"
      modifier "buildConfigCompilerRelease" = "compilerRelease"
      modifier a = a

instance FromJSON Compiler where
  parseJSON (String s) = maybe mzero pure (fromText s)
  parseJSON _ = fail "'Compiler' should be a String"

instance ToJSON Compiler where
  toJSON = toJSON . toText

compilerRepo :: Compiler -> GithubLocation
compilerRepo Purescript = GithubLocation (GithubOwner "purescript") (PackageName "purescript")

getBuildConfigs :: Manager -> AuthToken -> Compiler -> ReleaseFilter -> IO [BuildConfig]
getBuildConfigs manager token c releaseFilter =
  map (BuildConfig c) <$> getReleaseTags manager token (compilerRepo c) releaseFilter

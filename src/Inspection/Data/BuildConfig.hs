{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Inspection.Data.BuildConfig where

import           Control.Monad
import           Data.Data     (Data ())
import           Data.Map      (Map)
import           Data.Monoid
import           Data.Vector   (Vector)
import qualified Data.Text     as Text
import           Data.Typeable (Typeable ())
import           GHC.Generics  (Generic ())

import Data.Aeson.Extra
import Data.Aeson.Types    (Options (..), defaultOptions)
import Data.SafeCopy       (base, deriveSafeCopy)

import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Inspection.Data.PackageName
import Inspection.Data.ReleaseTag

import Inspection.GithubM

data BuildConfig
  = BuildConfig { buildConfigCompiler        :: Compiler
                , buildConfigCompilerRelease :: ReleaseTag Compiler
                }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Compiler = Purescript
              deriving (Eq, Show, Ord, Generic, Typeable, Data)

instance ToHttpApiData BuildConfig where
  toUrlPiece BuildConfig{..} = toUrlPiece buildConfigCompiler
                        <> "-"
                        <> toUrlPiece buildConfigCompilerRelease

instance FromHttpApiData BuildConfig where
  parseUrlPiece (Text.splitOn "-" -> [compiler, compilerRelease]) =
      BuildConfig <$> parseUrlPiece compiler
                  <*> parseUrlPiece compilerRelease
  parseUrlPiece _ = Left "Failed to parse BuildConfig"

instance ToJSONKey BuildConfig where
  toJSONKey = toUrlPiece

instance ToJSON a => ToJSON (Map BuildConfig a) where
  toJSON = toJSON . M

instance ToHttpApiData Compiler where
  toUrlPiece Purescript = "purescript"

instance FromHttpApiData Compiler where
  parseUrlPiece "purescript" = Right Purescript
  parseUrlPiece _ = Left "Unknown Compiler"

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
  parseJSON (String s) = either (const mzero) pure (parseUrlPiece s)
  parseJSON _ = fail "'Compiler' should be a String"

instance ToJSON Compiler where
  toJSON = toJSON . toUrlPiece

compilerRepo :: Compiler -> GithubLocation
compilerRepo Purescript = GithubLocation "purescript" "purescript"

getBuildConfigs :: Compiler -> ReleaseFilter -> GithubM (Vector BuildConfig)
getBuildConfigs c releaseFilter =
  fmap (BuildConfig c) <$> getReleaseTags (compilerRepo c) releaseFilter

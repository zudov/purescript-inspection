{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Inspection.BuildMatrix where

import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import Data.Maybe (fromMaybe)

import Data.Aeson.Extra
import Data.SafeCopy           (base, deriveSafeCopy)

import Inspection.BuildConfig
import Inspection.BuildResult
import Inspection.PackageName
import Inspection.ReleaseTag
import Inspection.Event
import Inspection.EventLog

data BuildMatrix
  = BuildMatrix (Map PackageName -- ^ Packages
                     (Map ReleaseTag -- ^ Package versions
                          (Map BuildConfig -- ^ Compiler versions
                               [BuildResult])))
  deriving (Show, Eq, Generic, Typeable)

deriveSafeCopy 0 'base ''BuildMatrix

instance ToJSON BuildMatrix where
  toJSON (BuildMatrix matrix) = toJSON $ M matrix

instance Monoid BuildMatrix where
  mempty = BuildMatrix mempty
  mappend (BuildMatrix a) (BuildMatrix b) =
    BuildMatrix (Map.unionWith (Map.unionWith (Map.unionWith (++))) a b)

execute :: Event -> BuildMatrix -> BuildMatrix
execute event@(AddBuildResult packageName releaseTag buildConfig buildResult) (BuildMatrix buildMatrix) =
    BuildMatrix $ Map.alter alterAtPackageName packageName buildMatrix
  where
    alterAtPackageName :: (v ~ Maybe (Map ReleaseTag (Map BuildConfig [BuildResult]))) => v -> v
    alterAtPackageName = Just . Map.alter alterAtReleaseTag releaseTag . fromMaybe mempty

    alterAtReleaseTag :: (v ~ Maybe (Map BuildConfig [BuildResult])) => v -> v
    alterAtReleaseTag = Just . Map.alter alterAtBuildConfig buildConfig . fromMaybe mempty

    alterAtBuildConfig :: (v ~ Maybe [BuildResult]) => v -> v
    alterAtBuildConfig = Just . (buildResult:) . fromMaybe []

restore :: EventLog Event -> BuildMatrix
restore eventLog = restore' eventLog mempty

restore' :: EventLog Event -> BuildMatrix -> BuildMatrix
restore' eventLog matrix = foldl (flip execute) matrix eventLog

packages :: BuildMatrix -> [PackageName]
packages (BuildMatrix matrix) = Map.keys matrix

compilers :: BuildMatrix -> [Compiler]
compilers (BuildMatrix matrix) =
  foldMap (foldMap (map buildConfigCompiler . Map.keys)) matrix

addReleaseTag :: PackageName -> ReleaseTag -> BuildMatrix -> BuildMatrix
addReleaseTag packageName releaseTag (BuildMatrix matrix) =
  BuildMatrix (Map.adjust (Map.insertWith Map.union releaseTag
                                         (Map.fromSet (const []) buildConfigs))
                           packageName matrix)
  where
    buildConfigs :: Set.Set BuildConfig
    buildConfigs = foldMap Map.keysSet (Map.elems =<< Map.elems matrix)

addBuildConfig :: BuildConfig -> BuildMatrix -> BuildMatrix
addBuildConfig buildConfig (BuildMatrix matrix) =
  BuildMatrix (fmap (fmap (Map.insertWith (++) buildConfig [])) matrix)

{-# LANGUAGE TemplateHaskell #-}
module Inspection.BuildMatrix where

import           Control.Monad (forM)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import Data.Aeson.Extra
import Data.SafeCopy           (base, deriveSafeCopy)
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Inspection.BuildConfig
import Inspection.BuildResult
import Inspection.PackageName
import Inspection.ReleaseTag

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

packages :: BuildMatrix -> [PackageName]
packages (BuildMatrix matrix) = Map.keys matrix

compilers :: BuildMatrix -> [Compiler]
compilers (BuildMatrix matrix) =
  foldMap (foldMap (map buildConfigCompiler . Map.keys)) matrix

populatedBuildMatrix :: [PackageName] -> BuildMatrix
populatedBuildMatrix =
  BuildMatrix . Map.fromSet (const Map.empty) . Set.fromList

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

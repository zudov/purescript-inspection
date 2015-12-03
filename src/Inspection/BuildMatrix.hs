{-# LANGUAGE TemplateHaskell #-}
module Inspection.BuildMatrix where

import           Control.Monad (forM)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import Data.Aeson.Extra
import Data.SafeCopy    (base, deriveSafeCopy)
import Network.Wreq     (withManager)

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

populatedBuildMatrix :: [PackageName] -> [Compiler] -> IO BuildMatrix
populatedBuildMatrix packages compilers = withManager $ \opts -> do
  buildConfigs <- concat <$> mapM (getBuildConfigs opts) compilers
  let buildConfigsMap = Map.fromList (zip (take 5 buildConfigs)
                                          (repeat []))
  BuildMatrix . Map.fromList <$>
    (forM packages $ \package -> do
       releaseTags <- getReleaseTags opts =<< getGithubLocation opts package
       pure (package, Map.fromList (zip (take 4 releaseTags)
                                        (repeat buildConfigsMap))))

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

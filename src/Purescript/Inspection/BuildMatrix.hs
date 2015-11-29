{-# LANGUAGE TemplateHaskell #-}
module Purescript.Inspection.BuildMatrix where

import Control.Monad
import Data.Aeson.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.SafeCopy
import Data.Typeable
import GHC.Generics

import Purescript.Inspection.ReleaseTag
import Purescript.Inspection.PackageName
import Purescript.Inspection.BuildResult
import Purescript.Inspection.BuildConfig

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

populatedBuildMatrix :: [PackageName] -> [Compiler] -> IO BuildMatrix
populatedBuildMatrix packages compilers = do
  buildConfigs <- concat <$> mapM getBuildConfigs compilers
  let buildConfigsMap = Map.fromList (zip (take 2 buildConfigs)
                                          (repeat []))
  BuildMatrix . Map.fromList <$>
    (forM packages $ \package -> do
       releaseTags <- getReleaseTags =<< getGithubLocation package
       pure (package, Map.fromList (zip (take 2 releaseTags)
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

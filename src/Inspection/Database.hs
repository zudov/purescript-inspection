{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Inspection.Database
  ( DB(..)
  , initialDB
  , GetBuildMatrix(..)
  , AddBuildResult(..)
  , AppendBuildMatrix(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Data.Monoid
import qualified Data.Map as Map
import GHC.Generics

import Inspection.BuildMatrix
import Inspection.BuildResult
import Inspection.ReleaseTag
import Inspection.PackageName
import Inspection.BuildConfig

data DB = DB { buildMatrix :: BuildMatrix
             } deriving (Show, Eq, Generic, Typeable)

initialDB :: DB
initialDB = DB { buildMatrix = mempty
               }

deriveSafeCopy 0 'base ''DB

getBuildMatrix :: Query DB BuildMatrix
getBuildMatrix = asks buildMatrix

appendBuildMatrix :: BuildMatrix -> Update DB ()
appendBuildMatrix matrix = modify (\db -> db { buildMatrix = buildMatrix db <> matrix })

addBuildResult :: PackageName -> ReleaseTag -> BuildConfig -> BuildResult
               -> Update DB (Maybe [BuildResult])
addBuildResult packageName packageVersion buildConfig result
    = state $ \db ->
        let matrix' = Map.adjust
                        (Map.adjust
                          (Map.adjust (result:) buildConfig)
                          packageVersion)
                        packageName (case buildMatrix db of BuildMatrix matrix -> matrix)
        in ( Map.lookup packageName matrix'
                >>= Map.lookup packageVersion
                  >>= Map.lookup buildConfig
           , db { buildMatrix = BuildMatrix matrix'
                }
           )

makeAcidic ''DB [ 'getBuildMatrix
                , 'appendBuildMatrix
                , 'addBuildResult
                ]

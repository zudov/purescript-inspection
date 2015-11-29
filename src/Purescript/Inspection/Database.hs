{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Purescript.Inspection.Database
  ( DB(..)
  , initialDB
  , GetBuildMatrix(..)
  , AddBuildResult(..)
  , AppendBuildMatrix(..)
  , GetTaskQueue(..)
  , AppendTaskQueue(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics

import Purescript.Inspection.BuildMatrix
import Purescript.Inspection.BuildResult
import Purescript.Inspection.Task
import Purescript.Inspection.TaskQueue
import Purescript.Inspection.Target
import Purescript.Inspection.ReleaseTag
import Purescript.Inspection.PackageName
import Purescript.Inspection.BuildConfig

data DB = DB { buildMatrix :: BuildMatrix
             , taskQueue   :: TaskQueue
             } deriving (Show, Eq, Generic, Typeable)

initialDB :: DB
initialDB = DB { buildMatrix = mempty
               , taskQueue = mempty
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
            queue' = Set.delete (Task buildConfig (Target packageName packageVersion))
                                (case taskQueue db of TaskQueue queue -> queue)
        in ( Map.lookup packageName matrix'
                >>= Map.lookup packageVersion
                  >>= Map.lookup buildConfig
           , db { buildMatrix = BuildMatrix matrix'
                , taskQueue = TaskQueue queue'
                }
           )

getTaskQueue :: Query DB TaskQueue
getTaskQueue = asks taskQueue

appendTaskQueue :: TaskQueue -> Update DB ()
appendTaskQueue queue = modify (\db -> db { taskQueue = taskQueue db <> queue })

makeAcidic ''DB [ 'getBuildMatrix
                , 'appendBuildMatrix
                , 'addBuildResult
                , 'getTaskQueue
                , 'appendTaskQueue
                ]

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Inspection.TaskQueue
  ( TaskQueue(..)
  , addTask
  , selectTasks
  , singleTask
  , completedTasks
  , difference
  ) where

import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Typeable (Typeable ())
import           GHC.Generics  (Generic ())

import Data.Aeson.Extra (ToJSON, FromJSON)

import Inspection.BuildConfig
import Inspection.BuildMatrix
import Inspection.PackageName
import Inspection.ReleaseTag
import Inspection.Target
import Inspection.Task

newtype TaskQueue = TaskQueue (Set Task)
                  deriving (Show, Eq, Generic, Typeable)

instance Monoid TaskQueue where
  mempty = TaskQueue mempty
  mappend (TaskQueue a) (TaskQueue b) = TaskQueue (mappend a b)

instance ToJSON TaskQueue
instance FromJSON TaskQueue

addTask :: Task -> TaskQueue -> TaskQueue
addTask task (TaskQueue queue) = TaskQueue $ Set.insert task queue

difference :: TaskQueue -> TaskQueue -> TaskQueue
difference (TaskQueue a) (TaskQueue b) = TaskQueue $ Set.difference a b

selectTasks :: Maybe Compiler
            -> Maybe ReleaseTag
            -> Maybe PackageName
            -> Maybe ReleaseTag
            -> TaskQueue -> TaskQueue
selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion (TaskQueue queue)
  = TaskQueue $ Set.filter match queue
  where
    match Task{ taskBuildConfig = BuildConfig{..}
              , taskTarget = Target packageName packageVersion
              } = and (fromMaybe True <$>
                         [ fmap (buildConfigCompiler ==)        mCompiler
                         , fmap (buildConfigCompilerRelease ==) mCompilerVersion
                         , fmap (packageName ==)     mPackageName
                         , fmap (packageVersion ==)  mPackageVersion
                         ])

singleTask :: TaskQueue -> Maybe Task
singleTask (TaskQueue queue) = fst <$> Set.minView queue

completedTasks :: BuildMatrix -> TaskQueue
completedTasks (BuildMatrix matrix) =
  TaskQueue
    (Map.foldMapWithKey
      (\packageName -> Map.foldMapWithKey
        (\versionTag -> Map.foldMapWithKey
          (\buildConfig results ->
            if null results
            then Set.empty
            else Set.singleton (Task buildConfig (Target packageName versionTag)))))
      matrix)

{-# LANGUAGE RecordWildCards #-}
module Inspection.Data.TaskQueue
  ( TaskQueue(..)
  , addTask
  , selectTasks
  , singleTask
  , completedTasks
  , difference
  ) where

import Prelude ()
import MyLittlePrelude

import qualified Data.Set      as Set

import Data.Aeson.Extra (ToJSON, FromJSON)

import Inspection.Data
import Inspection.BuildMatrix as BuildMatrix

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
            -> Maybe (ReleaseTag Compiler)
            -> Maybe PackageName
            -> Maybe (ReleaseTag Package)
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
completedTasks matrix =
  TaskQueue
    $ Set.map fromEntry
    $ BuildMatrix.entries Nothing Nothing Nothing Nothing matrix
  where
    fromEntry :: Entry -> Task
    fromEntry Entry{..} = Task entryBuildConfig entryTarget

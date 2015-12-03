{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Inspection.TaskQueue
  ( TaskQueue(..)
  , addTask
  , selectTasks
  , singleTask
  , allYourTasks
  ) where

import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Typeable (Typeable ())
import           GHC.Generics  (Generic ())

import Data.Aeson.Extra (ToJSON)

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

addTask :: Task -> TaskQueue -> TaskQueue
addTask task (TaskQueue queue) = TaskQueue $ Set.insert task queue

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
singleTask (TaskQueue queue)
  | Set.null queue = Nothing
  | otherwise = Just $ head $ Set.toList queue

allYourTasks :: Bool -> BuildMatrix -> TaskQueue
allYourTasks includeCompleted (BuildMatrix matrix) =
  TaskQueue
    (Map.foldMapWithKey
      (\packageName -> Map.foldMapWithKey
        (\versionTag -> Map.foldMapWithKey
          (\buildConfig results ->
            if null results || includeCompleted
            then Set.singleton (Task buildConfig (Target packageName versionTag))
            else Set.empty)))
      matrix)

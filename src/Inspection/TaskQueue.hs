{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Inspection.TaskQueue
  ( TaskQueue(..)
  , addTask
  , selectTasks
  , singleTask
  , allYourTasks
  ) where

import Data.Aeson.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import GHC.Generics

import Inspection.Task
import Inspection.Target
import Inspection.BuildConfig
import Inspection.PackageName
import Inspection.ReleaseTag
import Inspection.BuildMatrix

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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Inspection.API.Tasks
  ( TasksAPI
  , tasksServer
  ) where

import Prelude()
import MyLittlePrelude

import qualified Data.Set as Set

import Data.Acid as Acid

import Data.IORef
import qualified Data.Vector as Vector
import Network.HTTP.Client (Manager)
import Servant
import Control.Monad.Except

import GitHub as GH

import qualified Inspection.Config as Config

import Inspection.Data
import Inspection.Data.TaskQueue as TaskQueue
import Inspection.Data.BuildConfig (getBuildConfigs)
import Inspection.Data.ReleaseTag (getReleaseTags)
import Inspection.Flags
import Inspection.Data.AuthToken
import Inspection.API.Types
import Inspection.Database (GetBuildMatrix(..))
import Inspection.GithubM

type TasksAPI =
  QueryParam "compiler" Compiler
    :> QueryParam "compilerVersion" (ReleaseTag Compiler)
       :> QueryParam "packageName" PackageName
          :> QueryParam "packageVersion" (ReleaseTag Package)
            :> QueryFlag "includeCompleted"
               :> Get '[JSON] TaskQueue

tasksServer :: ServerT TasksAPI Inspector
tasksServer = getQueue

getQueue
  :: Maybe Compiler -> Maybe (ReleaseTag Compiler)
  -> Maybe PackageName -> Maybe (ReleaseTag Package)
  -> Bool
  -> Inspector TaskQueue
getQueue mCompiler mCompilerVersion mPackageName mPackageVersion includeCompleted = do
  Environment{..} <- ask
  tasks <- liftGithubM
         $ syncTaskQueue 
             (maybe (Config.compilers envConfig) (:[]) mCompiler)
             (maybe (Config.packages envConfig) (:[])
             ((`Config.packageLocation` envConfig) =<< mPackageName))
             (Config.releaseFilter envConfig)
  let selectedTasks = selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion tasks
  if includeCompleted
    then pure selectedTasks
    else TaskQueue.difference selectedTasks . TaskQueue.completedTasks <$> liftIO (query envAcid GetBuildMatrix)

syncTaskQueue
  :: [Compiler] -> [GithubLocation] -> ReleaseFilter
  -> GithubM TaskQueue
syncTaskQueue compilers githubLocations releaseFilter =
  (TaskQueue . Set.fromList . Vector.toList)
       <$> (liftA2 Task <$> syncBuildConfigs compilers releaseFilter
                        <*> syncTargets githubLocations releaseFilter)

syncBuildConfigs :: [Compiler] -> ReleaseFilter -> GithubM (Vector BuildConfig)
syncBuildConfigs compilers releaseFilter =
  mconcat <$> mapM (`getBuildConfigs` releaseFilter) compilers

syncTargets :: [GithubLocation] -> ReleaseFilter -> GithubM (Vector Target)
syncTargets locations releaseFilter = mconcat <$> mapM fromGithubLocation locations
  where
    fromGithubLocation l@(GithubLocation _ name) =
      fmap (Target name) <$> getReleaseTags l releaseFilter

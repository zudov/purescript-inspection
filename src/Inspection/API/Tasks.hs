{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Inspection.API.Tasks
  ( TasksAPI
  , tasksServer
  ) where

import Control.Concurrent.Async   (mapConcurrently)
import Control.Monad.Reader (ask, liftIO)

import qualified Data.Set as Set
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)

import Data.Acid as Acid

import Network.HTTP.Client (Manager)
import Servant

import qualified Inspection.Config as Config
import Inspection.TaskQueue
import Inspection.ReleaseTag

import Inspection.BuildConfig
import Inspection.PackageName

import Inspection.Flags
import Inspection.AuthToken
import Inspection.API.Types
import Inspection.Target
import Inspection.Task
import Inspection.Database (GetBuildMatrix(..))
import qualified Inspection.TaskQueue as TaskQueue

type TasksAPI =
  QueryParam "compiler" Compiler
    :> QueryParam "compilerVersion" ReleaseTag
       :> QueryParam "packageName" PackageName
          :> QueryParam "packageVersion" ReleaseTag
            :> QueryFlag "includeCompleted"
               :> Get '[JSON] TaskQueue

tasksServer :: ServerT TasksAPI Inspector
tasksServer = getQueue

getQueue :: Maybe Compiler -> Maybe ReleaseTag -> Maybe PackageName -> Maybe ReleaseTag
         -> Bool -> Inspector TaskQueue
getQueue mCompiler mCompilerVersion mPackageName mPackageVersion includeCompleted = do
  Environment{..} <- ask
  tasks <- liftIO $ syncTaskQueue
             envManager (githubAuthToken envFlags)
             (maybe (Config.compilers envConfig) (:[]) mCompiler)
             (maybe (Config.packages envConfig) (:[])
                    ((flip Config.packageLocation envConfig) =<< mPackageName))
             (Config.releaseFilter envConfig)
  let selectedTasks = selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion tasks
  if includeCompleted
    then pure selectedTasks
    else TaskQueue.difference selectedTasks . TaskQueue.completedTasks <$> liftIO (query envAcid GetBuildMatrix)
         
  

syncTaskQueue :: Manager -> AuthToken -> [Compiler] -> [GithubLocation] -> ReleaseFilter -> IO TaskQueue
syncTaskQueue manager token compilers githubLocations releaseFilter = do
  targets <- syncTargets manager token githubLocations releaseFilter
  buildConfigs <- syncBuildConfigs manager token compilers releaseFilter
  pure $ TaskQueue $ Set.fromList
                       [ Task buildConfig target
                           | target <- targets
                           , buildConfig <- buildConfigs ]

syncBuildConfigs :: Manager -> AuthToken -> [Compiler] -> ReleaseFilter -> IO [BuildConfig]
syncBuildConfigs manager token compilers releaseFilter =
  concat <$> mapConcurrently (\c -> getBuildConfigs manager token c releaseFilter) compilers

syncTargets :: Manager -> AuthToken -> [GithubLocation] -> ReleaseFilter -> IO [Target]
syncTargets manager token locations releaseFilter =
  concat <$> mapConcurrently (\(l@(GithubLocation _ name)) ->
                                 map (Target name)
                                   <$> getReleaseTags manager token l releaseFilter) locations

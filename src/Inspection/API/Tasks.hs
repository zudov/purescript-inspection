{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Inspection.API.Tasks
  ( TasksAPI
  , tasksServer
  ) where


import Control.Monad.Reader (ask, liftIO)

import qualified Data.Set as Set

import Data.Acid as Acid

import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Network.HTTP.Client (Manager)
import Servant
import Control.Monad.Except

import GitHub as GH

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
import Inspection.GithubM

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
  tasks <- lift $ withExceptT githubError $ syncTaskQueue
             envGithubCacheRef
             envManager (githubAuthToken envFlags)
             (maybe (Config.compilers envConfig) (:[]) mCompiler)
             (maybe (Config.packages envConfig) (:[])
                    ((flip Config.packageLocation envConfig) =<< mPackageName))
             (Config.releaseFilter envConfig)
  let selectedTasks = selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion tasks
  if includeCompleted
    then pure selectedTasks
    else TaskQueue.difference selectedTasks . TaskQueue.completedTasks <$> liftIO (query envAcid GetBuildMatrix)
         
  

syncTaskQueue
  :: IORef GithubCache -> Manager -> AuthToken -> [Compiler] -> [GithubLocation] -> ReleaseFilter
  -> ExceptT GH.Error IO TaskQueue
syncTaskQueue cacheRef manager token compilers githubLocations releaseFilter = do
  targets <- syncTargets cacheRef manager token githubLocations releaseFilter
  buildConfigs <- syncBuildConfigs cacheRef manager token compilers releaseFilter
  pure $ TaskQueue $ Set.fromList
                       [ Task buildConfig target
                           | target <- Vector.toList targets
                           , buildConfig <- Vector.toList buildConfigs ]

syncBuildConfigs
  :: IORef GithubCache -> Manager -> AuthToken -> [Compiler] -> ReleaseFilter
  -> ExceptT GH.Error IO (Vector BuildConfig)
syncBuildConfigs cacheRef manager token compilers releaseFilter =
  mconcat <$> mapM ((\c -> runGithubM cacheRef manager token $ getBuildConfigs c releaseFilter)) compilers

syncTargets
  :: IORef GithubCache -> Manager -> AuthToken -> [GithubLocation] -> ReleaseFilter
  -> ExceptT GH.Error IO (Vector Target)
syncTargets cacheRef manager token locations releaseFilter =
  mconcat <$> mapM (\(l@(GithubLocation _ name)) ->
                        fmap (fmap (Target name))
                          $ runGithubM cacheRef manager token
                          $ getReleaseTags l releaseFilter)
                    locations

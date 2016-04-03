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
  tasks <- lift $ withExceptT githubError $ syncTaskQueue
             envGithubCacheRef
             envManager (githubAuthToken envFlags)
             (maybe (Config.compilers envConfig) (:[]) mCompiler)
             (maybe (Config.packages envConfig) (:[])
                    (`Config.packageLocation` envConfig) =<< mPackageName)
             (Config.releaseFilter envConfig)
  let selectedTasks = selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion tasks
  if includeCompleted
    then pure selectedTasks
    else TaskQueue.difference selectedTasks . TaskQueue.completedTasks <$> liftIO (query envAcid GetBuildMatrix)
         
  

syncTaskQueue
  :: IORef GithubCache -> Manager -> AuthToken -> [Compiler] -> [GithubLocation] -> ReleaseFilter
  -> ExceptT GH.Error IO TaskQueue
syncTaskQueue cacheRef manager token compilers githubLocations releaseFilter = do
  liftIO $ putStrLn "Fetching targets"
  targets <- syncTargets cacheRef manager token githubLocations releaseFilter
  liftIO $ putStrLn "Fetching buildConfigs"
  buildConfigs <- syncBuildConfigs cacheRef manager token compilers releaseFilter
  pure $ TaskQueue $ Set.fromList
                       [ Task buildConfig target
                           | target <- Vector.toList targets
                           , buildConfig <- Vector.toList buildConfigs ]

syncBuildConfigs
  :: IORef GithubCache -> Manager -> AuthToken -> [Compiler] -> ReleaseFilter
  -> ExceptT GH.Error IO (Vector BuildConfig)
syncBuildConfigs cacheRef manager (toGithubAuth -> auth) compilers releaseFilter =
  mconcat <$> mapM (\c -> runGithubM cacheRef manager auth $ getBuildConfigs c releaseFilter) compilers

syncTargets
  :: IORef GithubCache -> Manager -> AuthToken -> [GithubLocation] -> ReleaseFilter
  -> ExceptT GH.Error IO (Vector Target)
syncTargets cacheRef manager (toGithubAuth -> auth) locations releaseFilter =
  mconcat <$> mapM (\(l@(GithubLocation _ name)) ->
                        fmap (fmap (Target name))
                          $ runGithubM cacheRef manager auth
                          $ getReleaseTags l releaseFilter)
                    locations

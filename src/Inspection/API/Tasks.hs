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
import Data.List (sort)

import Data.Acid (query, update)
import Network.HTTP.Client (Manager)
import Servant

import qualified Inspection.Config as Config
import Inspection.TaskQueue
import Inspection.ReleaseTag
import Inspection.Database
import Inspection.BuildConfig
import Inspection.PackageName
import Inspection.BuildMatrix
import Inspection.Flags
import Inspection.AuthToken
import Inspection.API.Types

type TasksAPI =
  QueryParam "compiler" Compiler
    :> QueryParam "compilerVersion" ReleaseTag
       :> QueryParam "packageName" PackageName
          :> QueryParam "packageVersion" ReleaseTag
            :> QueryFlag "rebuild"
               :> Get '[JSON] TaskQueue

tasksServer :: ServerT TasksAPI Inspector
tasksServer = getQueue

getQueue :: Maybe Compiler -> Maybe ReleaseTag -> Maybe PackageName -> Maybe ReleaseTag
         -> Bool -> Inspector TaskQueue
getQueue mCompiler mCompilerVersion mPackageName mPackageVersion rebuild = do
  Environment{..} <- ask
  matrix <- sync mCompiler mPackageName (githubAuthToken envFlags)
  pure $ selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion
       $ allYourTasks rebuild
       $ matrix


sync :: Maybe Compiler -> Maybe PackageName -> AuthToken -> Inspector BuildMatrix
sync mCompiler mPackageName token = do
  Environment{..} <- ask
  matrix <- liftIO (query envAcid GetBuildMatrix)
  packagesSynced <-
    liftIO (syncPackages
              envManager
              token
              (maybe (Config.packages envConfig) (:[])
                     (flip Config.packageLocation envConfig =<< mPackageName))
              matrix)
  liftIO (syncBuildConfigs
            envManager
            token
            (maybe (Config.compilers envConfig) (:[]) mCompiler)
            packagesSynced)

syncBuildConfigs :: Manager -> AuthToken -> [Compiler] -> BuildMatrix -> IO BuildMatrix
syncBuildConfigs manager token compilers matrix =
  foldr addBuildConfig matrix . concatMap (take 5 . reverse . sort)
    <$> mapConcurrently (getBuildConfigs manager token) compilers

syncPackages :: Manager -> AuthToken -> [GithubLocation] -> BuildMatrix -> IO BuildMatrix
syncPackages manager token locations matrix =
  foldr (\(name, tags) m -> foldr (addReleaseTag name) m (take 5 $ reverse $ sort tags)) matrix <$>
    mapConcurrently (\(l@(GithubLocation _ name)) -> (name,) <$> getReleaseTags manager token l) locations

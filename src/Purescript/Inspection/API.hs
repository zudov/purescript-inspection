{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Purescript.Inspection.API where

import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Acid
import Data.Map (Map)
import qualified Data.Map as Map
import Servant

import Purescript.Inspection.Database
import Purescript.Inspection.BuildMatrix
import Purescript.Inspection.ReleaseTag
import Purescript.Inspection.BuildConfig
import Purescript.Inspection.BuildResult
import Purescript.Inspection.PackageName
import Purescript.Inspection.TaskQueue

type Inspector = ReaderT (AcidState DB) (EitherT ServantErr IO)

inspectorToEither' :: AcidState DB -> Inspector a
                   -> EitherT ServantErr IO a
inspectorToEither' acid r = runReaderT r acid

inspectorToEither :: AcidState DB -> Inspector :~> EitherT ServantErr IO
inspectorToEither acid = Nat (inspectorToEither' acid)

type InspectorAPI =
       "matrix" :> BuildMatrixAPI
  :<|> "queue"  :> QueueAPI

inspectorServer :: ServerT InspectorAPI Inspector
inspectorServer = buildMatrixServer
             :<|> queueServer

type BuildMatrixAPI =
       Get '[JSON] BuildMatrix
  :<|> Capture "packageName" PackageName
          :> Get '[JSON] (Map ReleaseTag
                            (Map BuildConfig
                                 [BuildResult]))
  :<|> Capture "packageName" PackageName
       :> Capture "packageVersion" ReleaseTag
          :> Capture "compiler" Compiler
             :> Capture "compilerVersion" ReleaseTag
                :> "success" :> Post '[JSON] [BuildResult]
  :<|> Capture "packageName" PackageName
       :> Capture "packageVersion" ReleaseTag
          :> Capture "compiler" Compiler
             :> Capture "compilerVersion" ReleaseTag
                :> "failure" :> Post '[JSON] [BuildResult]

buildMatrixServer :: ServerT BuildMatrixAPI Inspector
buildMatrixServer = getBuildMatrix
               :<|> getPackageReleases
               :<|> addBuildResult Success
               :<|> addBuildResult Failure

getBuildMatrix :: Inspector BuildMatrix
getBuildMatrix = do
  acid <- ask
  liftIO (query acid GetBuildMatrix)

getPackageReleases :: PackageName -> Inspector (Map ReleaseTag
                                                  (Map BuildConfig
                                                     [BuildResult]))
getPackageReleases packageName = do
  acid <- ask
  BuildMatrix matrix <- liftIO (query acid GetBuildMatrix)
  case Map.lookup packageName matrix of
    Just a  -> pure a
    Nothing -> lift (left err404)

addBuildResult :: BuildResult
               -> PackageName -> ReleaseTag -> Compiler -> ReleaseTag
               -> Inspector [BuildResult]
addBuildResult result packageName packageVersion compiler compilerVersion = do
  acid <- ask
  mResults <- liftIO (update acid (AddBuildResult packageName packageVersion
                                                  (BuildConfig compiler compilerVersion)
                                                  result))
  case mResults of
    Nothing -> lift (left err404)
    Just results -> pure results


type QueueAPI =
      QueryParam "compiler" Compiler
      :> QueryParam "compilerVersion" ReleaseTag
         :> QueryParam "packageName" PackageName
            :> QueryParam "packageVersion" ReleaseTag
               :> Get '[JSON] TaskQueue
 :<|> QueryParam "compiler" Compiler
      :> QueryParam "compilerVersion" ReleaseTag
         :> QueryParam "packageName" PackageName
            :> QueryParam "packageVersion" ReleaseTag
              :> QueryFlag "rebuild"
                 :> Post '[JSON] TaskQueue

queueServer :: ServerT QueueAPI Inspector
queueServer = getQueue
         :<|> addToQueue

getQueue :: Maybe Compiler -> Maybe ReleaseTag -> Maybe PackageName -> Maybe ReleaseTag
         -> Inspector TaskQueue
getQueue mCompiler mCompilerVersion mPackageName mPackageVersion = do
  acid <- ask
  tasks <- liftIO (query acid GetTaskQueue)
  pure $ selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion
       $ tasks

addToQueue :: Maybe Compiler -> Maybe ReleaseTag -> Maybe PackageName -> Maybe ReleaseTag
           -> Bool -> Inspector TaskQueue
addToQueue mCompiler mCompilerVersion mPackageName mPackageVersion rebuild = do
  acid <- ask
  matrix <- liftIO (query acid GetBuildMatrix)
  let newTasks = selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion
               $ allYourTasks rebuild
               $ matrix
  liftIO $ update acid (AppendTaskQueue newTasks)
  pure newTasks 

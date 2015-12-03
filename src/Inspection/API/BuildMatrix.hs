{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Inspection.API.BuildMatrix
  ( BuildMatrixAPI
  , buildMatrixServer
  ) where

import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Map (Map())
import qualified Data.Map as Map
import Data.Acid

import Servant

import Inspection.ReleaseTag
import Inspection.BuildConfig
import Inspection.BuildResult
import Inspection.BuildMatrix
import Inspection.Database
import Inspection.PackageName

import Inspection.API.Types

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
  acid <- asks envAcid
  liftIO (query acid GetBuildMatrix)

getPackageReleases :: PackageName -> Inspector (Map ReleaseTag
                                                  (Map BuildConfig
                                                     [BuildResult]))
getPackageReleases packageName = do
  acid <- asks envAcid
  BuildMatrix matrix <- liftIO (query acid GetBuildMatrix)
  case Map.lookup packageName matrix of
    Just a  -> pure a
    Nothing -> lift (left err404)

addBuildResult :: BuildResult
               -> PackageName -> ReleaseTag -> Compiler -> ReleaseTag
               -> Inspector [BuildResult]
addBuildResult result packageName packageVersion compiler compilerVersion = do
  acid <- asks envAcid
  mResults <- liftIO (update acid (AddBuildResult packageName packageVersion
                                                  (BuildConfig compiler compilerVersion)
                                                  result))
  case mResults of
    Nothing -> lift (left err404)
    Just results -> pure results


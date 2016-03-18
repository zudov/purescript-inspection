{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Inspection.API.BuildMatrix
  ( BuildMatrixAPI
  , AddBuildResultAPI
  , buildMatrixServer
  ) where

import           Control.Monad.Reader (ask, asks, lift, liftIO)
import           Control.Monad.Trans.Either
import           Data.Map                   (Map ())
import qualified Data.Map                   as Map
import           Data.Time.Clock (getCurrentTime)
import           Data.Aeson
import Data.Acid (query, update)
import Servant
import Data.Function

import Inspection.API.Types
import Inspection.BuildConfig
import Inspection.BuildMatrix
import Inspection.BuildResult
import Inspection.Database
import Inspection.PackageName
import Inspection.ReleaseTag
import Inspection.Event (Event(AddBuildResult))
import Inspection.EventLog (EventRecord(..), EventId)
import Inspection.AuthToken
import Inspection.Config

type BuildMatrixAPI =
       Get '[JSON] BuildMatrix
  :<|> Capture "packageName" PackageName
          :> Get '[JSON] (Map ReleaseTag
                            (Map BuildConfig
                                 [BuildResult]))
  :<|> AddBuildResultAPI

type AddBuildResultAPI
  = Header "Authorization" AuthToken
    :> Capture "packageName" PackageName
       :> Capture "packageVersion" ReleaseTag
          :> Capture "compiler" Compiler
             :> Capture "compilerVersion" ReleaseTag
                :> ReqBody '[JSON] BuildResult
                   :> Post '[JSON] EventId

buildMatrixServer :: ServerT BuildMatrixAPI Inspector
buildMatrixServer = getBuildMatrix
               :<|> getPackageReleases
               :<|> addBuildResult

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

addBuildResult :: Maybe AuthToken -> PackageName -> ReleaseTag -> Compiler -> ReleaseTag -> BuildResult
               -> Inspector EventId
addBuildResult Nothing _ _ _ _ _ =
  lift $ left err401 { errBody = encode $ object
                        ["errors" .= [ "Authorization token missing" :: String ]] }
addBuildResult (Just authToken) packageName packageVersion compiler compilerVersion result = do
  Environment{..} <- ask
  case packageLocation packageName envConfig of
    Nothing -> lift $ left $ err404 {errBody = encode $ object
                       [ "errors" .= [ "The package wasn't added to inspection" :: String ]]}
    Just githubLocation -> do
      packageVersions <- liftIO $ getReleaseTags envManager authToken githubLocation defaultReleaseFilter
      if packageVersion `notElem` packageVersions
        then
          lift $ left $ err404 { errBody = encode $ object
                   [ "errors" .= [ "Unknown package version" :: String ]]}
        else do
          buildConfigs <- liftIO $ getBuildConfigs envManager authToken compiler defaultReleaseFilter
          let buildConfig = BuildConfig compiler compilerVersion
          if buildConfig `notElem` buildConfigs
             then
               lift $ left $ err404 { errBody = encode $ object
                        [ "errors" .= [ "Unknown compiler version" :: String ]]}
             else do
              let event = AddBuildResult packageName packageVersion buildConfig result
              currentTime <- liftIO getCurrentTime
              liftIO $ update envAcid $ AddEventRecord $ EventRecord currentTime event

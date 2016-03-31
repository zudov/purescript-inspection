{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Inspection.API.BuildMatrix
  ( BuildMatrixAPI
  , AddBuildResultAPI
  , buildMatrixServer
  , AddBuildResultBody(..)
  ) where

import Prelude ()
import MyLittlePrelude

import           Control.Monad.Reader (asks)
import           Control.Monad.Except
import           Data.Time.Clock (getCurrentTime)
import           Data.Aeson
import           Data.ByteString.Base64.Type (ByteString64)
import Data.Acid (query, update)
import Servant
import Servant.HTML.Lucid
import Lucid as Lucid (ToHtml(..))
import Lucid.Html5

import qualified Data.Vector as Vector

import Inspection.Data
import Inspection.Data.ReleaseTag (getReleaseTags, defaultReleaseFilter)
import Inspection.Event (Event(..))
import Inspection.API.Types
import Inspection.BuildMatrix as BuildMatrix
import Inspection.Database
import Inspection.EventLog (EventRecord(..), EventId)
import Inspection.Config
import Inspection.GithubM
import Inspection.Data.BuildConfig (getBuildConfigs)
import Inspection.Data.AuthToken (toGithubAuth)
import qualified Inspection.BuildLogStorage as BuildLogStorage

type BuildMatrixAPI =
       GetBuildResultsAPI
  :<|> AddBuildResultAPI

type GetBuildResultsAPI =
  QueryParam "compiler" Compiler
    :> QueryParam "compilerVersion" (ReleaseTag Compiler)
       :> QueryParam "packageName" PackageName
          :> QueryParam "packageVersion" (ReleaseTag Package)
             :> Get '[JSON, HTML] GetBuildResults

type AddBuildResultAPI
  = Header "Authorization" AuthToken
    :> Capture "packageName" PackageName
       :> Capture "packageVersion" (ReleaseTag Package)
          :> Capture "compiler" Compiler
             :> Capture "compilerVersion" (ReleaseTag Compiler)
                :> ReqBody '[JSON] AddBuildResultBody
                   :> Post '[JSON] EventId

newtype GetBuildResults
  = GetBuildResults { runGetBuildResults :: Set BuildMatrix.Entry }

instance ToJSON GetBuildResults where
  toJSON = toJSON . runGetBuildResults

instance ToHtml GetBuildResults where
  toHtml (GetBuildResults entries) = do
    table_ $ do
      thead_ $ do
        tr_ $ do
          th_ "Target"
          th_ "Compiler"
          th_ "Result"
      tbody_ $ do
        forM_ entries $ \(Entry{entryTarget = Target{..}, entryBuildConfig = BuildConfig{..},..}) -> do
          tr_ $ do
            td_ $ do
              toHtml targetPackageName
              "-"
              toHtml targetReleaseTag
            td_ $ do
              toHtml $ toUrlPiece $ buildConfigCompiler
              "-"
              toHtml $ buildConfigCompilerRelease
            td_ $ do
              toHtml $ show entryBuildResult
    
buildMatrixServer :: ServerT BuildMatrixAPI Inspector
buildMatrixServer = getBuildResults
               :<|> addBuildResult

getBuildResults
  :: Maybe Compiler -> Maybe (ReleaseTag Compiler)
  -> Maybe PackageName -> Maybe (ReleaseTag Package)
  -> Inspector GetBuildResults
getBuildResults mCompiler mCompilerVersion mPackageName mPackageVersion = do
  acid <- asks envAcid
  buildMatrix <- liftIO (query acid GetBuildMatrix)
  pure $ GetBuildResults $
    BuildMatrix.entries
      mCompiler mCompilerVersion
      mPackageName mPackageVersion
      buildMatrix

data AddBuildResultBody
  = AddBuildResultBody
      { buildResult :: BuildResult
      , buildLogs   :: Vector (BuildLogStorage.BuildLog ByteString64)
      }
  deriving (Generic)

instance FromJSON AddBuildResultBody
instance ToJSON AddBuildResultBody

addBuildResult
  :: Maybe AuthToken
  -> PackageName -> ReleaseTag Package
  -> Compiler -> ReleaseTag Compiler
  -> AddBuildResultBody
  -> Inspector EventId
addBuildResult Nothing _ _ _ _ _ = do
  liftIO $ putStrLn "Nope"
  throwError err401 { errBody = encode $ object
                        ["errors" .= [ "Authorization token missing" :: String ]] }
addBuildResult (Just (toGithubAuth -> auth)) packageName packageVersion compiler compilerVersion AddBuildResultBody{..} = do
  Environment{..} <- ask
  case packageLocation packageName envConfig of
    Nothing -> throwError $ err404 {errBody = encode $ object
                       [ "errors" .= [ "The package wasn't added to inspection" :: String ]]}
    Just githubLocation -> do
      packageVersions <- lift $ withExceptT githubError
                              $ runGithubM envGithubCacheRef envManager auth
                           (getReleaseTags githubLocation defaultReleaseFilter)
      if packageVersion `notElem` Vector.toList packageVersions
        then
          throwError $ err404 { errBody = encode $ object
                   [ "errors" .= [ "Unknown package version" :: String ]]}
        else do
          buildConfigs <- lift $ withExceptT githubError
                               $ runGithubM envGithubCacheRef envManager auth
                            (getBuildConfigs compiler defaultReleaseFilter)
          let buildConfig = BuildConfig compiler compilerVersion
          if buildConfig `notElem` buildConfigs
             then
               throwError $ err404 { errBody = encode $ object
                        [ "errors" .= [ "Unknown compiler version" :: String ]]}
             else do
              _ <- BuildLogStorage.putBuildLogs
                     envBuildLogStorageEnv
                     (compiler, compilerVersion, packageName, packageVersion)
                     buildLogs
                     
              let event = AddBuildResult packageName packageVersion buildConfig buildResult
              currentTime <- liftIO getCurrentTime
              liftIO $ update envAcid $ AddEventRecord $ EventRecord currentTime event

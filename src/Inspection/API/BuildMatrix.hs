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
import Lucid (ToHtml(..))
import Lucid.Html5
import qualified Data.Text as Text

import Inspection.Data
import Inspection.Data.ReleaseTag (getRelease)
import Inspection.Event (Event(..))
import Inspection.API.Types
import Inspection.BuildMatrix as BuildMatrix
import Inspection.Database
import Inspection.EventLog (EventRecord(..), EventId)
import Inspection.Config
import Inspection.GithubM
import Inspection.Data.BuildConfig (compilerRepo)
import Inspection.Data.AuthToken (toGithubAuth)
import Inspection.BuildLogStorage (BuildLog(..), Command(..), CommandLog(..))
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
  toHtml (GetBuildResults entries') =
    table_ $ do
      thead_ $
        tr_ $ do
          th_ "Target"
          th_ "Compiler"
          th_ "Result"
          th_ "Logs"
      tbody_ $
        forM_ entries' $ \Entry{entryTarget = Target{..}, entryBuildConfig = BuildConfig{..},..} ->
          tr_ $ do
            td_ $ do
              toHtml targetPackageName
              "-"
              toHtml targetReleaseTag
            td_ $ do
              toHtml $ toUrlPiece buildConfigCompiler
              "-"
              toHtml buildConfigCompilerRelease
            td_ $
              toHtml $ show entryBuildResult
            td_ $
              ul_ $
                forM_ entryLogs $
                  \BuildLog{ buildLogCommand = Command{..}
                           , buildLogCommandLog = CommandLog{..} } ->
                  li_ $ do
                    span_ [ title_ command] $ toHtml commandIdentifier
                    " "
                    toHtml ("(ExitCode: " <> show exitCode <> ")")
                    " -- "
                    forM_ stdout (\url -> a_ [ href_ (Text.pack url) ] "stdout")
                    " "
                    forM_ stderr (\url -> a_ [ href_ (Text.pack url) ] "stderr")
                    

                    
                
  toHtmlRaw = toHtml
    
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
      , buildLogs   :: Vector (BuildLog ByteString64)
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
addBuildResult Nothing _ _ _ _ _ =
  throwError err401 { errBody = encode $ object
                        ["errors" .= [ "Authorization token missing" :: String ]] }
addBuildResult (Just (toGithubAuth -> auth)) packageName packageTag compiler compilerTag AddBuildResultBody{..} = do
  Environment{..} <- ask
  case packageLocation packageName envConfig of
    Nothing -> throwError $ err404 {errBody = encode $ object
                       [ "errors" .= [ "The package wasn't added to inspection" :: String ]]}
    Just githubLocation -> do
      mPackageRelease <- lift $ withExceptT githubError
                              $ runGithubM envGithubCacheRef envManager auth
                                           (getRelease githubLocation packageTag)
      case mPackageRelease of
        Nothing ->
          throwError $ err404 { errBody = encode $ object
                   [ "errors" .= [ "Unknown package version" :: String ]]}
        Just packageRelease -> do
          mCompilerRelease <- lift $ withExceptT githubError
                                   $ runGithubM envGithubCacheRef envManager auth
                                       (getRelease (compilerRepo compiler) compilerTag)
          case mCompilerRelease of
             Nothing ->
               throwError $ err404 { errBody = encode $ object
                        [ "errors" .= [ "Unknown compiler version" :: String ]]}
             Just compilerRelease -> do
              buildLogs_ <- BuildLogStorage.putBuildLogs
                     envBuildLogStorageEnv
                     (compiler, compilerTag, packageName, packageTag)
                     buildLogs
                     
              let event = AddBuildResult packageName packageTag (BuildConfig compiler compilerTag) buildResult buildLogs_
              currentTime <- liftIO getCurrentTime
              liftIO $ update envAcid $ AddEventRecord $ EventRecord currentTime event

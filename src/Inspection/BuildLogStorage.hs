{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase#-}
module Inspection.BuildLogStorage where

import MyLittlePrelude

import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import qualified Data.Text as Text

import Data.ByteString.Base64.Type (getByteString64, ByteString64)

import Network.HTTP.Conduit (RequestBody(..))
import Network.HTTP.Client (Manager)

import Web.HttpApiData (toUrlPiece)

import qualified Aws
import qualified Aws.Core as Aws
import Aws.S3 as S3

import Inspection.Data

data Config
  = Config
      { awsConfiguration :: Aws.Configuration
      , s3Configuration  :: S3.S3Configuration Aws.NormalQuery
      , s3Bucket :: S3.Bucket
      }

data Environment
  = Environment
      { envManager :: Manager
      , envConfig  :: Config
      }

loadConfig
  :: (MonadIO m)
  => m Config
loadConfig = do
  creds <- loadAwsCredentials
  pure $ Config
    { awsConfiguration = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug)
    , s3Configuration  = S3.s3 Aws.HTTPS "s3-eu-west-1.amazonaws.com" False
    , s3Bucket = "build-logs.inspection.purescript.org"
    }
  where
    loadAwsCredentials = Aws.loadCredentialsFromEnv >>= \case
      Nothing -> error "Missing AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY environment variables."
      Just a -> pure a

data Command
  = Command
      { commandIdentifier :: Text -- ^ Unique command identifier, used in the filenames
      , command           :: Text -- ^ A (pseudo-)shell command
      }
  deriving (Eq, Ord, Generic, Typeable)

instance FromJSON Command
instance ToJSON Command

data CommandLog a
  = CommandLog
    { stdout   :: Maybe a
    , stderr   :: Maybe a
    , exitCode :: Int
    }
  deriving (Generic, Typeable)

instance FromJSON a => FromJSON (CommandLog a)
instance ToJSON a => ToJSON (CommandLog a)

data BuildLog a
  = BuildLog
      { buildLogCommand    :: Command
      , buildLogCommandLog :: CommandLog a
      }
  deriving (Generic, Typeable)

instance FromJSON a => FromJSON (BuildLog a)
instance ToJSON a => ToJSON (BuildLog a)

putBuildLogs
  :: forall m. (MonadIO m)
  => Environment
  -> (Compiler, ReleaseTag Compiler, PackageName, ReleaseTag Package)
  -> Vector (BuildLog ByteString64)
  -> m (Vector (BuildLog String))
putBuildLogs (Environment{envConfig = Config{..}, ..}) entry buildLogs = do
  liftIO $ runResourceT $ do
    forM_ buildLogs $ \(BuildLog{buildLogCommand = Command{..}, ..}) ->
      forM (catMaybes
              [ ("stdout",) <$> stdout buildLogCommandLog
              , ("stderr",) <$> stderr buildLogCommandLog ]) $ \(logType, content) ->
          Aws.pureAws
            awsConfiguration s3Configuration envManager $
            putFile s3Bucket
              (entryToFilename entry <> "/" <> commandIdentifier <> "/" <> logType)
              [ ("command-id", commandIdentifier)
              , ("command", command)
              , ("log-type", logType)
              , ("exit-code", toUrlPiece (exitCode buildLogCommandLog))
              ]
              content
  pure $ fmap resolveBuildLog buildLogs
  where
    resolveBuildLog :: BuildLog a -> BuildLog String
    resolveBuildLog buildLog =
      BuildLog
        (buildLogCommand buildLog)
        (CommandLog
           (storedLogURL s3Bucket (entryToFilename entry) "stdout" <$ stdout (buildLogCommandLog buildLog))
           (storedLogURL s3Bucket (entryToFilename entry) "stderr" <$ stderr (buildLogCommandLog buildLog))
           (exitCode (buildLogCommandLog buildLog)))
    putFile :: S3.Bucket -> Text -> [(Text, Text)] -> ByteString64 -> S3.PutObject
    putFile bucket fileName metadata content =
      (S3.putObject bucket fileName
        (RequestBodyBS $ getByteString64 content))
        { S3.poMetadata = entryMetadata entry <> metadata
        , S3.poAcl = Just S3.AclPublicRead
        , S3.poContentType = Just "text/plain"
        }

storedLogURL :: S3.Bucket -> Text -> String -> String
storedLogURL bucket filename logType =
  "https://" <> Text.unpack bucket <> ".s3-website-eu-west-1.amazonaws.com/"
             <> Text.unpack filename <> "/" <> logType
  

entryToFilename :: (Compiler, ReleaseTag Compiler, PackageName, ReleaseTag Package) -> Text
entryToFilename (compiler, compilerVersion, package, packageVersion) =
  toUrlPiece package <> "/" <> toUrlPiece packageVersion <> "/" <>
  toUrlPiece compiler <> "/" <> toUrlPiece compilerVersion

entryMetadata :: (Compiler, ReleaseTag Compiler, PackageName, ReleaseTag Package) -> [(Text, Text)]
entryMetadata (compiler, compilerVersion, package, packageVersion) =
  [ ("package", toUrlPiece package)
  , ("package-version", toUrlPiece packageVersion)
  , ("compiler", toUrlPiece compiler)
  , ("compiler-version", toUrlPiece compilerVersion)
  ]

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Inspection.API.Sync 
  ( SyncAPI
  , syncAPI
  ) where

import Control.Monad.Reader (asks, liftIO)
import Control.Lens
import qualified Data.Text.Encoding as Text

import Data.Acid 
import Data.Aeson (Value())
import Servant
import Network.Wreq

import Inspection.AuthToken
import Inspection.Database
import Inspection.BuildMatrix
import Inspection.ReleaseTag
import Inspection.BuildConfig
import Inspection.BuildResult
import Inspection.PackageName
import Inspection.TaskQueue

import Inspection.API.Types

type SyncAPI =
  Header "Authorization" AuthToken :> Get '[JSON] Value

syncAPI (Just token) = do
  let opts = defaults & header "Authorization" .~ [Text.encodeUtf8 (toText token)]
  response <- liftIO (asJSON =<< getWith opts "https://api.github.com/user")
  pure (response ^. responseBody)

syncMatrix :: Inspector ()
syncMatrix = do
  acid <- asks envAcid
  matrix <- liftIO (query acid GetBuildMatrix)
  populated <- liftIO (populatedBuildMatrix (packages matrix)
                                            (compilers matrix))
  liftIO (update acid (AppendBuildMatrix populated))

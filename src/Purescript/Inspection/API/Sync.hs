{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Purescript.Inspection.API.Sync 
  ( SyncAPI
  , syncAPI
  ) where

import Control.Monad.Reader
import Control.Lens
import Data.Acid
import Data.Aeson (Value())
import qualified Data.Text.Encoding as Text

import Servant
import Network.Wreq

import Purescript.Inspection.AuthToken
import Purescript.Inspection.Database
import Purescript.Inspection.BuildMatrix
import Purescript.Inspection.ReleaseTag
import Purescript.Inspection.BuildConfig
import Purescript.Inspection.BuildResult
import Purescript.Inspection.PackageName
import Purescript.Inspection.TaskQueue

import Purescript.Inspection.API.Types

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

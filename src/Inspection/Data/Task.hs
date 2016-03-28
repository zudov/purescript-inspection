{-# LANGUAGE TemplateHaskell #-}
module Inspection.Data.Task where

import Prelude ()
import MyLittlePrelude

import Data.Aeson.Extra (ToJSON (..), genericToJSON, FromJSON(..), genericParseJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.SafeCopy    (base, deriveSafeCopy)

import Inspection.Data.BuildConfig
import Inspection.Data.Target

newtype TaskId = TaskId Int deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Task = Task { taskBuildConfig :: BuildConfig
                 , taskTarget      :: Target
                 }
          deriving (Show, Eq, Ord, Generic, Typeable, Data)

instance ToJSON Task where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modifier }
    where
      modifier "taskBuildConfig" = "buildConfig"
      modifier "taskTarget" = "target"
      modifier a = a

instance FromJSON Task where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modifier }
    where
      modifier "taskBuildConfig" = "buildConfig"
      modifier "taskTarget" = "target"
      modifier a = a

deriveSafeCopy 0 'base ''Task

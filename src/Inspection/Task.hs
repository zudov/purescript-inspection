{-# LANGUAGE TemplateHaskell #-}
module Inspection.Task where

import Data.Data     (Data ())
import Data.Typeable (Typeable ())
import GHC.Generics  (Generic ())

import Data.Aeson.Extra (ToJSON (..), genericToJSON)
import Data.Aeson.Types (Options (..), defaultOptions)
import Data.SafeCopy    (base, deriveSafeCopy)

import Inspection.BuildConfig
import Inspection.Target

newtype TaskId = TaskId Int deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Task = Task { taskBuildConfig :: BuildConfig
                 , taskTarget      :: Target
                 }
          deriving (Show, Eq, Ord, Generic, Typeable)

instance ToJSON Task where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = modifier }
    where
      modifier "taskBuildConfig" = "buildConfig"
      modifier "taskTarget" = "target"
      modifier a = a

deriveSafeCopy 0 'base ''Task

{-# LANGUAGE TemplateHaskell #-}
module Inspection.Task where

import Data.Aeson.Extra
import Data.Aeson.Types
import Data.Typeable (Typeable())
import Data.Data (Data())
import Data.SafeCopy
import GHC.Generics (Generic())

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

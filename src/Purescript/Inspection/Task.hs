module Purescript.Inspection.Task where

import Data.Typeable (Typeable())
import Data.Data (Data())
import GHC.Generics (Generic())

import Purescript.Inspection.BuildConfig
import Purescript.Inspection.Target

newtype TaskId = TaskId Int deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Task = Task { taskBuildConfig :: BuildConfig
                 , taskTarget      :: Target
                 }
          deriving (Show, Eq, Ord, Generic, Typeable)

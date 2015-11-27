module Purescript.Inspection.Task where

import Data.Typeable (Typeable())
import GHC.Generics (Generic())

import Purescript.Inspection.BuildConfig
import Purescript.Inspection.Target

data Task = Task { taskBuildConfig :: BuildConfig
                 , taskTarget      :: Target
                 }
          deriving (Show, Eq, Ord, Generic, Typeable)

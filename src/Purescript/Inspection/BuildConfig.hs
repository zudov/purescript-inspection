module Purescript.Inspection.BuildConfig where

import Data.Typeable (Typeable())
import Data.Data (Data())
import GHC.Generics (Generic())

import Purescript.Inspection.ReleaseTag

data BuildConfig
  = BuildConfig { pscVersion :: ReleaseTag }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

{-# LANGUAGE TemplateHaskell #-}
module Inspection.Event where

import Data.SafeCopy (deriveSafeCopy, base)

import Inspection.PackageName
import Inspection.ReleaseTag
import Inspection.BuildConfig
import Inspection.BuildResult

data Event
  = AddBuildResult PackageName ReleaseTag BuildConfig BuildResult
  deriving (Show, Eq)
deriveSafeCopy 0 'base ''Event

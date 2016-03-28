{-# LANGUAGE TemplateHaskell #-}
module Inspection.Event where

import Data.SafeCopy (deriveSafeCopy, base)

import Inspection.Data

data Event
  = AddBuildResult PackageName (ReleaseTag Package) BuildConfig BuildResult
  deriving (Show, Eq)
           
deriveSafeCopy 0 'base ''Event

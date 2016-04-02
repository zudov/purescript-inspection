{-# LANGUAGE TemplateHaskell #-}
module Inspection.Event where

import Prelude ()
import MyLittlePrelude

import Data.SafeCopy (deriveSafeCopy, base)

import Inspection.Data
import Inspection.BuildLogStorage

data Event
  = AddBuildResult PackageName (ReleaseTag Package) BuildConfig BuildResult (Vector (BuildLog String))
  deriving (Show, Eq)
           
deriveSafeCopy 0 'base ''Event

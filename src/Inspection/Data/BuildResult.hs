{-# LANGUAGE TemplateHaskell #-}
module Inspection.Data.BuildResult where

import Prelude ()
import MyLittlePrelude

import Data.Aeson.Extra
import Data.SafeCopy    (base, deriveSafeCopy)

data BuildResult
  = Success
  | Warnings
  | Failure
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

deriveSafeCopy 0 'base ''BuildResult

instance ToJSON BuildResult
instance FromJSON BuildResult

{-# LANGUAGE TemplateHaskell #-}
module Inspection.BuildResult where

import Data.Data     (Data ())
import Data.Typeable (Typeable ())
import GHC.Generics  (Generic ())

import Data.Aeson.Extra
import Data.SafeCopy    (base, deriveSafeCopy)

data BuildResult = Success
                 | Failure
                 deriving (Show, Eq, Ord, Generic, Typeable, Data)

deriveSafeCopy 0 'base ''BuildResult

instance ToJSON BuildResult

{-# LANGUAGE TemplateHaskell #-}
module Purescript.Inspection.BuildResult where

import Data.Aeson.Extra
import Data.Typeable (Typeable())
import Data.Data (Data())
import GHC.Generics (Generic())
import Data.SafeCopy

data BuildResult = Success
                 | Failure
                 deriving (Show, Eq, Ord, Generic, Typeable, Data)

deriveSafeCopy 0 'base ''BuildResult

instance ToJSON BuildResult

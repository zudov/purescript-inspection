{-# LANGUAGE TemplateHaskell #-}
module Inspection.BuildResult where

import Data.Data     (Data ())
import Data.Text     (Text())
import Data.Typeable (Typeable ())
import GHC.Generics  (Generic ())

import Data.Aeson.Extra
import Data.SafeCopy    (base, deriveSafeCopy)

data BuildResult = Success
                 | Warnings Text
                 | Failure Text
                 deriving (Show, Eq, Ord, Generic, Typeable, Data)

deriveSafeCopy 0 'base ''BuildResult

instance ToJSON BuildResult
instance FromJSON BuildResult

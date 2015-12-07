{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Inspection.Target where

import Data.Typeable (Typeable ())
import GHC.Generics  (Generic ())

import Data.Aeson.Extra (ToJSON, FromJSON)
import Data.SafeCopy (deriveSafeCopy, base)

import Inspection.PackageName
import Inspection.ReleaseTag

data Target = Target PackageName ReleaseTag
            deriving (Show, Eq, Ord, Generic, Typeable)

instance ToJSON Target
instance FromJSON Target

deriveSafeCopy 0 'base ''Target

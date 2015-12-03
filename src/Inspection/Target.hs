{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Inspection.Target where

import Data.Typeable (Typeable ())
import GHC.Generics  (Generic ())

import Data.Aeson.Extra (ToJSON)
import Data.SafeCopy (deriveSafeCopy, base)

import Inspection.PackageName
import Inspection.ReleaseTag

data Target = Target PackageName ReleaseTag
            deriving (Show, Eq, Ord, Generic, Typeable)

instance ToJSON Target

deriveSafeCopy 0 'base ''Target

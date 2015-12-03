{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Inspection.Target where

import Data.Aeson.Extra
import Data.Typeable (Typeable())
import Data.SafeCopy
import GHC.Generics (Generic())

import Inspection.PackageName

import Inspection.ReleaseTag

data Target = Target PackageName ReleaseTag
            deriving (Show, Eq, Ord, Generic, Typeable)

instance ToJSON Target

deriveSafeCopy 0 'base ''Target

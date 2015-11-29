{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Purescript.Inspection.Target where

import Data.Aeson.Extra
import Data.Typeable (Typeable())
import Data.SafeCopy
import GHC.Generics (Generic())

import Purescript.Inspection.PackageName

import Purescript.Inspection.ReleaseTag

data Target = Target PackageName ReleaseTag
            deriving (Show, Eq, Ord, Generic, Typeable)

instance ToJSON Target

deriveSafeCopy 0 'base ''Target

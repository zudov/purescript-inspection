{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Inspection.Data.Target where

import MyLittlePrelude

import Data.Aeson.Extra (ToJSON, FromJSON)
import Data.SafeCopy (deriveSafeCopy, base)

import Inspection.Data.PackageName (PackageName)
import Inspection.Data.ReleaseTag (ReleaseTag)
import Inspection.Data.Package (Package)

data Target
  = Target
      { targetPackageName :: PackageName
      , targetReleaseTag :: ReleaseTag Package
      }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

instance ToText Target where
  toText (Target packageName packageVersion) =
    toText packageName <> "-" <> toText packageVersion

instance ToJSON Target
instance FromJSON Target

deriveSafeCopy 0 'base ''Target

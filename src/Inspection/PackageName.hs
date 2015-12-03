{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Inspection.PackageName
  (PackageName) where

import Data.Map (Map)
import qualified Data.Text as Text

import Data.Aeson.Extra
import Data.SafeCopy (deriveSafeCopy, base)
import Servant.Common.Text (FromText(..))

import Web.Bower.PackageMeta (PackageName, runPackageName, mkPackageName)

deriveSafeCopy 0 'base ''PackageName

instance ToJSONKey PackageName where
  toJSONKey = Text.pack . runPackageName

instance ToJSON a => ToJSON (Map PackageName a) where
  toJSON = toJSON . M

instance FromText PackageName where
  fromText = either (const Nothing) Just . mkPackageName . Text.unpack

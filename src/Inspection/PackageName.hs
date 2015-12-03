{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Inspection.PackageName
  (PackageName) where

import Data.Aeson.Extra
import Data.Map (Map)
import Data.SafeCopy
import qualified Data.Text as Text

import Servant.Common.Text

import Web.Bower.PackageMeta

deriveSafeCopy 0 'base ''PackageName

instance ToJSONKey PackageName where
  toJSONKey = Text.pack . runPackageName

instance ToJSON a => ToJSON (Map PackageName a) where
  toJSON = toJSON . M

instance FromText PackageName where
  fromText = either (const Nothing) Just . mkPackageName . Text.unpack

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Inspection.PackageName
  ( PackageName(..)
  ) where

import Data.Map  (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Data (Data)

import Data.Aeson.Extra
import Data.SafeCopy       (base, deriveSafeCopy)
import Servant.Common.Text (FromText(..), ToText(..))

newtype PackageName = PackageName { runPackageName :: Text }
                    deriving (Show, Eq, Ord, Generic, Typeable, Data)

deriveSafeCopy 0 'base ''PackageName

instance ToJSON PackageName where
  toJSON = toJSON . runPackageName

instance FromJSON PackageName where
  parseJSON = fmap PackageName . parseJSON

instance ToJSONKey PackageName where
  toJSONKey = runPackageName

instance ToJSON a => ToJSON (Map PackageName a) where
  toJSON = toJSON . M

instance FromText PackageName where
  fromText = Just . PackageName

instance ToText PackageName where
  toText = runPackageName

{-# LANGUAGE OverloadedStrings #-}
module Purescript.Inspection.AuthToken
  ( AuthToken()
  , runAuthToken
  ) where

import Data.Text (Text())
import qualified Data.Text as Text
import Servant.Common.Text

newtype AuthToken = AuthToken { runAuthToken :: Text } deriving (Show, Eq)

instance FromText AuthToken where
  fromText t = case Text.words t of
    ["token", token] -> Just (AuthToken token)
    _                -> Nothing

instance ToText AuthToken where
  toText (AuthToken token) = Text.unwords ["token", token]

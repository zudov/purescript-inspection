{-# LANGUAGE OverloadedStrings #-}
module Inspection.AuthToken
  ( AuthToken(..)
  ) where


import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.ByteString (ByteString)


import Servant.Common.Text

newtype AuthToken = AuthToken { runAuthToken :: ByteString } deriving (Show, Eq)

instance FromText AuthToken where
  fromText t = case Text.words t of
    ["token", token] -> Just (AuthToken (encodeUtf8 token))
    _                -> Nothing

instance ToText AuthToken where
  toText (AuthToken token) = Text.unwords ["token", decodeUtf8 token]

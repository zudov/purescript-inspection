{-# LANGUAGE OverloadedStrings #-}
module Inspection.Data.AuthToken
  ( AuthToken(..)
  , toGithubAuth
  ) where

import MyLittlePrelude

import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified GitHub as GH

import Servant.Common.Text (FromText(..))

newtype AuthToken = AuthToken { runAuthToken :: ByteString } deriving (Show, Eq, Generic)

instance Hashable AuthToken

instance FromText AuthToken where
  fromText t = case Text.words t of
    ["token", token] -> Just (AuthToken (encodeUtf8 token))
    _                -> Nothing

instance ToText AuthToken where
  toText (AuthToken token) = Text.unwords ["token", decodeUtf8 token]

toGithubAuth :: AuthToken -> GH.Auth
toGithubAuth (AuthToken t) = GH.OAuth t

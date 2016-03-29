{-# LANGUAGE OverloadedStrings #-}
module Inspection.Data.AuthToken
  ( AuthToken(..)
  , toGithubAuth
  ) where

import MyLittlePrelude

import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified GitHub as GH

import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

newtype AuthToken = AuthToken { runAuthToken :: ByteString } deriving (Show, Eq, Generic)

instance Hashable AuthToken

instance FromHttpApiData AuthToken where
  parseUrlPiece t = case Text.words t of
    ["token", token] -> Right (AuthToken (encodeUtf8 token))
    _                -> Left "Failed to parse AuthToken"

instance ToHttpApiData AuthToken where
  toUrlPiece (AuthToken token) = Text.unwords ["token", decodeUtf8 token]

toGithubAuth :: AuthToken -> GH.Auth
toGithubAuth (AuthToken t) = GH.OAuth t

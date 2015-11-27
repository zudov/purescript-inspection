{-# LANGUAGE OverloadedStrings #-}
module Purescript.Inspection.ReleaseTag
  ( ReleaseTag()
  , getReleaseTags
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable())
import Data.Data (Data())
import Data.Monoid ((<>))
import Data.Aeson.Lens
import GHC.Generics (Generic())
import Control.Lens
import Network.Wreq

-- | Release tags as they appear in Github's releases
newtype ReleaseTag = ReleaseTag Text deriving (Show, Eq, Ord, Generic, Typeable, Data)

newtype GithubOwner = GithubOwner Text deriving (Show, Eq, Ord, Generic, Typeable, Data)
newtype GithubRepo  = GithubRepo  Text deriving (Show, Eq, Ord, Generic, Typeable, Data)

getReleaseTags :: GithubOwner -> GithubRepo -> IO [ReleaseTag]
getReleaseTags (GithubOwner owner) (GithubRepo repo) = do
  response <- get url
  pure (ReleaseTag <$> (response ^.. responseBody . values . key "tag_name" . _String))
  where
    url = "https://api.github.com/repos/" <> Text.unpack owner <> "/"
                                          <> Text.unpack repo <> "/releases"

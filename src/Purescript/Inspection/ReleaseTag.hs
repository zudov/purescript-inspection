{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Purescript.Inspection.ReleaseTag
  ( ReleaseTag(..)
  , GithubLocation(..)
  , GithubOwner(..)
  , GithubRepo(..)
  , getReleaseTags
  , getGithubLocation
  ) where

import Data.Aeson.Extra
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import Data.Maybe
import Data.Typeable (Typeable())
import Data.SafeCopy
import Data.Data (Data())
import Data.Monoid ((<>))
import Data.Aeson.Lens
import GHC.Generics (Generic())
import Control.Lens
import Servant.Common.Text
import Network.Wreq

import Web.Bower.PackageMeta (PackageName(), runPackageName)
import Network.URI

-- | Release tags as they appear in Github's releases
newtype ReleaseTag = ReleaseTag { runReleaseTag :: Text }
                   deriving (Show, Eq, Ord, Generic, Typeable, Data)

instance ToJSONKey ReleaseTag where
  toJSONKey = runReleaseTag

instance ToJSON a => ToJSON (Map ReleaseTag a) where
  toJSON = toJSON . M

instance ToText ReleaseTag where
  toText = runReleaseTag

instance FromText ReleaseTag where
  fromText = Just . ReleaseTag

deriveSafeCopy 0 'base ''ReleaseTag

instance ToJSON ReleaseTag where
  toJSON = toJSON . runReleaseTag 

data GithubLocation = GithubLocation GithubOwner GithubRepo
                    deriving (Show, Eq, Ord, Generic, Typeable, Data)
newtype GithubOwner = GithubOwner Text
                    deriving (Show, Eq, Ord, Generic, Typeable, Data)
newtype GithubRepo = GithubRepo Text
                   deriving (Show, Eq, Ord, Generic, Typeable, Data)

getReleaseTags :: GithubLocation -> IO [ReleaseTag]
getReleaseTags (GithubLocation (GithubOwner owner) (GithubRepo repo)) = do
  response <- get url
  pure (ReleaseTag <$> (response ^.. responseBody . values . key "tag_name" . _String))
  where
    url = "https://api.github.com/repos/"
            <> escapeURIString isUnreserved (Text.unpack owner) <> "/"
            <> escapeURIString isUnreserved (Text.unpack repo) <> "/releases"

getGithubLocation :: PackageName -> IO GithubLocation
getGithubLocation (runPackageName -> packageName) = do
  [gitUrl] <- (^.. responseBody . key "url" . _String) <$> get url
  [_,GithubOwner -> owner, GithubRepo -> repo] <- either fail pure
    (Text.splitOn "/" <$>
      ((\x -> Right (fromMaybe x (Text.stripSuffix ".git" x)))
         =<< maybe (Left "Cannot parse github url") (Right . Text.pack)
                   (uriPath <$> parseURI (Text.unpack gitUrl))))
  pure (GithubLocation owner repo)
  where
    url = "https://bower.herokuapp.com/packages/" <> escapeURIString isUnreserved packageName

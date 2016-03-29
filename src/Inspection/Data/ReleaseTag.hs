{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Inspection.Data.ReleaseTag
  ( ReleaseTag(..)
  , mkReleaseTag
  , Release(..)
  , GithubLocation(..)
  , GithubOwner(..)
  , anonymous
  , getReleases
  , getReleaseTags
  , ReleaseFilter(..)
  , defaultReleaseFilter
  ) where

import Prelude ()
import MyLittlePrelude

import qualified Data.Vector        as Vector
import qualified Data.Text          as Text

import           Data.Aeson.Extra
import           Data.SafeCopy       (SafeCopy(..), contain, safePut, safeGet)

import           Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

import qualified GitHub as GH


import Inspection.Data.PackageName
import Inspection.GithubM

data Release entity
  = Release
      { releaseTag         :: ReleaseTag entity
      , releasePublishedAt :: UTCTime
      }
  deriving (Show, Eq, Ord)

instance FromJSON (Release entity) where
  parseJSON (Object o) = Release <$> (ReleaseTag <$> o .: "tag_name")
                                 <*> (o .: "published_at")
  parseJSON _ = mzero

-- | Release tags as they appear in Github's releases
newtype ReleaseTag entity
  = ReleaseTag { runReleaseTag :: Text }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

mkReleaseTag :: proxy entity -> Text -> ReleaseTag entity
mkReleaseTag _ = ReleaseTag

instance ToJSONKey (ReleaseTag entity) where
  toJSONKey = runReleaseTag

instance ToJSON a => ToJSON (Map (ReleaseTag entity) a) where
  toJSON = toJSON . M

instance ToHttpApiData (ReleaseTag entity) where
  toUrlPiece = runReleaseTag

instance FromHttpApiData (ReleaseTag entity) where
  parseUrlPiece = Right . ReleaseTag

instance SafeCopy (ReleaseTag entity) where
  putCopy = contain . safePut . runReleaseTag
  getCopy = contain $ mkReleaseTag (Proxy :: Proxy entity) <$> safeGet

instance ToJSON (ReleaseTag entity) where
  toJSON = toJSON . runReleaseTag

instance FromJSON (ReleaseTag entity) where
  parseJSON = fmap ReleaseTag . parseJSON

data GithubLocation
  = GithubLocation { githubOwner :: GithubOwner
                   , githubPackageName :: PackageName
                   }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

newtype GithubOwner = GithubOwner Text
                    deriving (Show, Eq, Ord, Generic, Typeable, Data)

anonymous :: GithubOwner
anonymous = GithubOwner "anonymous"

instance FromJSON GithubLocation where
  parseJSON (String (Text.splitOn "/" -> [owner, packageName])) =
    pure (GithubLocation (GithubOwner owner) (PackageName packageName))
  parseJSON _ = mzero

data ReleaseFilter
  = ReleaseFilter { publishedAfter :: Maybe UTCTime }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ReleaseFilter where
  parseJSON = withObject "ReleaseFilter" $ \o ->
    ReleaseFilter <$> (o .:? "publishedAfter")

defaultReleaseFilter :: ReleaseFilter
defaultReleaseFilter = ReleaseFilter
  { publishedAfter = Nothing }

matchReleaseFilter :: ReleaseFilter -> Release entity -> Bool
matchReleaseFilter ReleaseFilter{..} Release{..} =
  maybe True (releasePublishedAt >) publishedAfter

getReleases :: GithubLocation -> ReleaseFilter -> GithubM (Vector (Release entity))
getReleases (GithubLocation (GithubOwner owner) (PackageName repo)) releaseFilter =
  Vector.filter (matchReleaseFilter releaseFilter) <$>
    githubRequest
      (GH.PagedQuery ["repos", Text.unpack owner, Text.unpack repo, "releases"]
                     []
                     Nothing)

getReleaseTags :: GithubLocation -> ReleaseFilter -> GithubM (Vector (ReleaseTag entity))
getReleaseTags location releaseFilter =
  fmap releaseTag <$> getReleases location releaseFilter

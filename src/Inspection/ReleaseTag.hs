{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Inspection.ReleaseTag
  ( ReleaseTag(..)
  , Release(..)
  , GithubLocation(..)
  , GithubOwner(..)
  , getReleases
  , getReleaseTags
  , ReleaseFilter(..)
  , defaultReleaseFilter
  ) where


import           Control.Monad      (mzero)
import           Data.Data          (Data ())

import           Data.Map           (Map)
import           Data.Vector        (Vector)
import qualified Data.Vector        as Vector

import           Data.Text          (Text)
import qualified Data.Text          as Text

import           Data.Time.Clock    (UTCTime)
import           Data.Typeable      (Typeable ())
import           GHC.Generics       (Generic ())

import           Data.Aeson.Extra

import           Data.SafeCopy       (base, deriveSafeCopy)



import           Servant.Common.Text (FromText (..), ToText (..))

import qualified GitHub as GH


import Inspection.PackageName
import Inspection.GithubM

data Release = Release { releaseTag         :: ReleaseTag
                       , releasePublishedAt :: UTCTime
                       }
             deriving (Show, Eq, Ord)

instance FromJSON Release where
  parseJSON (Object o) = Release <$> (ReleaseTag <$> o .: "tag_name")
                                 <*> (o .: "published_at")
  parseJSON _ = mzero

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

instance FromJSON ReleaseTag where
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

matchReleaseFilter :: ReleaseFilter -> Release -> Bool
matchReleaseFilter ReleaseFilter{..} Release{..} =
  maybe True (releasePublishedAt >) publishedAfter

getReleases :: (Monad m) => GithubLocation -> ReleaseFilter -> GithubT m (Vector Release)
getReleases (GithubLocation (GithubOwner owner) (PackageName repo)) releaseFilter =
  Vector.filter (matchReleaseFilter releaseFilter) <$>
    githubRequest
      (GH.PagedQuery ["repos", Text.unpack owner, Text.unpack repo, "releases"]
                     []
                     Nothing)

getReleaseTags :: (Monad m) => GithubLocation -> ReleaseFilter -> GithubT m (Vector ReleaseTag)
getReleaseTags location releaseFilter =
  fmap releaseTag <$> getReleases location releaseFilter

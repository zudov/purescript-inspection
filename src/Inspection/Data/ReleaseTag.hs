{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Inspection.Data.ReleaseTag
  ( ReleaseTag
  , mkReleaseTag
  , IsReleaseTag
  , Release(..)
  , GithubLocation(..)
  , GithubOwner
  , IsGithubOwner
  , getReleases
  , getReleaseTags
  , ReleaseFilter(..)
  , defaultReleaseFilter
  ) where

import Prelude ()
import MyLittlePrelude

import qualified Data.Vector        as Vector
import qualified Data.Text          as Text

import Data.Aeson.Extra
import Web.HttpApiData (toUrlPiece)

import Refined.Extended

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
  parseJSON (Object o) = do
    tag_name <- o .: "tag_name"
    published_at <- o .: "published_at"
    either fail pure $ Release <$> refine tag_name <*> published_at
  parseJSON _ = mzero

data IsReleaseTag entity deriving Typeable
deriving instance Data entity => Data (IsReleaseTag entity)

instance Predicate (IsReleaseTag entity) Text where
  validate _ _ = Nothing

type ReleaseTag entity = Refined (IsReleaseTag entity) Text 

mkReleaseTag :: proxy entity -> Text -> ReleaseTag entity
mkReleaseTag _ = either error id . refine

data GithubLocation
  = GithubLocation { githubOwner :: GithubOwner
                   , githubPackageName :: PackageName
                   }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

data IsGithubOwner deriving (Typeable)
deriving instance Data IsGithubOwner

instance Predicate IsGithubOwner Text where
  validate _ _ = Nothing

type GithubOwner = Refined IsGithubOwner Text

instance FromJSON GithubLocation where
  parseJSON (String (Text.splitOn "/" -> [owner, packageName])) =
      either fail pure (GithubLocation <$> refine owner <*> refine packageName)
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
getReleases (GithubLocation owner repo) releaseFilter =
  Vector.filter (matchReleaseFilter releaseFilter) <$>
    githubRequest
      (GH.PagedQuery (Text.unpack <$> ["repos", toUrlPiece owner, toUrlPiece repo, "releases"])
                     []
                     Nothing)

getReleaseTags :: GithubLocation -> ReleaseFilter -> GithubM (Vector (ReleaseTag entity))
getReleaseTags location releaseFilter =
  fmap releaseTag <$> getReleases location releaseFilter

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
  ) where

import           Control.Lens
import           Control.Monad      (mzero)
import           Data.Data          (Data ())
import           Data.Function      ((&))
import           Data.Map           (Map)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock    (UTCTime)
import           Data.Typeable      (Typeable ())
import           GHC.Generics       (Generic ())

import           Data.Aeson.Extra
import           Data.Aeson.Lens
import           Data.SafeCopy       (base, deriveSafeCopy)
import           Network.HTTP.Client (Manager)
import           Network.URI         (escapeURIString, isUnreserved)
import qualified Network.Wreq        as Wreq
import           Servant.Common.Text (FromText (..), ToText (..))

import Inspection.AuthToken
import Inspection.PackageName

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

data GithubLocation = GithubLocation { githubOwner :: GithubOwner
                                     , githubPackageName :: PackageName
                                     }
                    deriving (Show, Eq, Ord, Generic, Typeable, Data)
newtype GithubOwner = GithubOwner Text
                    deriving (Show, Eq, Ord, Generic, Typeable, Data)

instance FromJSON GithubLocation where
  parseJSON (String (Text.splitOn "/" -> [owner, packageName])) =
    pure (GithubLocation (GithubOwner owner) (PackageName packageName))
  parseJSON _ = mzero

getReleases :: Manager -> AuthToken -> GithubLocation -> IO [Release]
getReleases manager token (GithubLocation (GithubOwner owner) (PackageName repo)) = do
  response <- Wreq.getWith opts url
  decode (response ^. Wreq.responseBody)
  where
    url = "https://api.github.com/repos/"
            <> escapeURIString isUnreserved (Text.unpack owner) <> "/"
            <> escapeURIString isUnreserved (Text.unpack repo) <> "/releases"
    opts = Wreq.defaults
             & Wreq.manager .~ Right manager
             & Wreq.header "Authorization" .~ [Text.encodeUtf8 (toText token)]

getReleaseTags :: Manager -> AuthToken -> GithubLocation -> IO [ReleaseTag]
getReleaseTags manager token location = map releaseTag <$> getReleases manager token location

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Inspection.ReleaseTag
  ( ReleaseTag(..)
  , GithubLocation(..)
  , GithubOwner(..)
  , GithubRepo(..)
  , getReleaseTags
  , getGithubLocation
  ) where

import           Control.Lens  ((.~), (^..))
import           Data.Data     (Data ())
import           Data.Function ((&))
import           Data.Map      (Map)
import           Data.Maybe    (fromMaybe)
import           Data.Monoid   ((<>))
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Data.Typeable (Typeable ())
import           GHC.Generics  (Generic ())

import           Data.Aeson.Extra
import           Data.Aeson.Lens       (key, values, _String)
import           Data.SafeCopy         (base, deriveSafeCopy)
import           Network.HTTP.Client   (Manager)
import           Network.URI           (escapeURIString, isUnreserved, parseURI,
                                        uriPath)
import qualified Network.Wreq          as Wreq
import           Servant.Common.Text   (FromText (..), ToText (..))
import           Web.Bower.PackageMeta (PackageName (), runPackageName)

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

getReleaseTags :: Manager -> GithubLocation -> IO [ReleaseTag]
getReleaseTags manager (GithubLocation (GithubOwner owner) (GithubRepo repo)) = do
  response <- Wreq.getWith opts url
  pure (ReleaseTag <$> (response ^.. Wreq.responseBody . values . key "tag_name" . _String))
  where
    url = "https://api.github.com/repos/"
            <> escapeURIString isUnreserved (Text.unpack owner) <> "/"
            <> escapeURIString isUnreserved (Text.unpack repo) <> "/releases"
    opts = Wreq.defaults & Wreq.manager .~ Right manager

getGithubLocation :: Manager -> PackageName -> IO GithubLocation
getGithubLocation manager (runPackageName -> packageName) = do
  [gitUrl] <- (^.. Wreq.responseBody . key "url" . _String) <$> Wreq.getWith opts url
  putStrLn $ show gitUrl
  [_,GithubOwner -> owner, GithubRepo -> repo] <- either fail pure
    (Text.splitOn "/" <$>
      ((\x -> Right (fromMaybe x (Text.stripSuffix ".git" x)))
         =<< maybe (Left "Cannot parse github url") (Right . Text.pack)
                   (uriPath <$> parseURI (Text.unpack gitUrl))))
  pure (GithubLocation owner repo)
  where
    url = "https://bower.herokuapp.com/packages/" <> escapeURIString isUnreserved packageName
    opts = Wreq.defaults & Wreq.manager .~ Right manager

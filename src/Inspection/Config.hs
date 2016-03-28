module Inspection.Config where

import Data.List (find)
import GHC.Generics (Generic ())

import Data.Aeson.Extra
import Data.Yaml (decodeFileEither)

import Inspection.Data

data Config
  = Config { compilers     :: [Compiler]
           , packages      :: [GithubLocation]
           , releaseFilter :: ReleaseFilter
           }
  deriving (Show, Generic)

getConfig :: FilePath -> IO Config
getConfig fp = either (fail . show) pure =<< decodeFileEither fp

packageNames :: Config -> [PackageName]
packageNames = map (\(GithubLocation _ n) -> n ) . packages

packageLocation :: PackageName -> Config -> Maybe GithubLocation
packageLocation name =
  find (\(GithubLocation _ name') -> name == name') . packages

instance FromJSON Config

module Inspection.Flags
  ( Flags(..)
  , getEnvironmentFlags
  ) where

import qualified Data.ByteString.Char8 as ByteString.Char8

import System.Environment (getEnv)

import Inspection.AuthToken

data Flags = Flags { githubAuthToken :: AuthToken
                   }

getEnvironmentFlags :: IO Flags
getEnvironmentFlags =
  Flags <$> (AuthToken . ByteString.Char8.pack <$> getEnv "INSPECTION_GITHUB_AUTH_TOKEN")

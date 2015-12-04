module Inspection.Flags
  ( Flags(..)
  , getEnvironmentFlags
  ) where

import qualified Data.Text as Text

import System.Environment (getEnv)

import Inspection.AuthToken

data Flags = Flags { githubAuthToken :: AuthToken
                   }

getEnvironmentFlags :: IO Flags
getEnvironmentFlags =
  Flags <$> (AuthToken . Text.pack <$> getEnv "INSPECTION_GITHUB_AUTH_TOKEN")

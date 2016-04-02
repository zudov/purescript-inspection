{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Inspection.API.Types
  ( Environment(..)
  , Inspector
  , inspectorToEither
  , githubError
  ) where

import Control.Monad.Reader       (ReaderT ())
import Control.Monad.Trans.Either (EitherT ())
import Control.Monad.Except       (ExceptT())
import Control.Category

import Data.Acid           (AcidState ())
import Data.IORef
import Network.HTTP.Client (Manager)
import Servant
import Data.Aeson.Extra

import Inspection.Database
import Inspection.Flags
import Inspection.Config
import Inspection.GithubM
import qualified GitHub as GH
import qualified Inspection.BuildLogStorage as BuildLogStorage

data Environment
  = Environment
      { envAcid    :: AcidState DB
      , envManager :: Manager
      , envFlags   :: Flags
      , envConfig  :: Config
      , envGithubCacheRef :: IORef GithubCache
      , envBuildLogStorageEnv :: BuildLogStorage.Environment 
      }

type Inspector = ReaderT Environment (ExceptT ServantErr IO)

inspectorToEither :: Environment -> Inspector :~> ExceptT ServantErr IO
inspectorToEither env = runReaderTNat env

githubError :: GH.Error -> ServantErr
githubError e = err500 { errBody = encode $ object
                          ["errors" .= [ "Failure when querying the GitHub API." :: String
                                       , show e
                                       ]]}

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
import GitHub as GH

data Environment
  = Environment { envAcid    :: AcidState DB
                , envManager :: Manager
                , envFlags   :: Flags
                , envConfig  :: Config
                , envGithubCacheRef :: IORef GithubCache
                }

type Inspector = ReaderT Environment (ExceptT ServantErr IO)

inspectorToEither :: Environment -> Inspector :~> EitherT ServantErr IO
inspectorToEither env = runReaderTNat env >>> fromExceptT

githubError :: GH.Error -> ServantErr
githubError _ = err500 { errBody = encode $ object
                          ["errors" .= ["Failure when querying the GitHub API." :: String]]}

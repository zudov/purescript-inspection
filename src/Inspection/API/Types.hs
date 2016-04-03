{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Inspection.API.Types
  ( Environment(..)
  , Inspector
  , inspectorToEither
  , liftGithubM
  , liftGithubMAuth
  , githubError
  ) where

import Prelude ()
import MyLittlePrelude

import Control.Monad.Reader       (ReaderT (), ask)
import Control.Monad.Trans.Either (EitherT ())
import Control.Monad.Except       (ExceptT(), withExceptT)
import Control.Monad.Trans        (lift)
import Control.Category

import Data.Acid           (AcidState ())
import Data.IORef
import Network.HTTP.Client (Manager)
import Servant
import Data.Aeson.Extra

import Inspection.Database
import Inspection.Flags
import Inspection.Data.AuthToken (AuthToken, toGithubAuth)
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
inspectorToEither = runReaderTNat

liftGithubM :: GithubM a -> Inspector a
liftGithubM m = do
  Environment{..} <- ask
  lift $ withExceptT githubError
       $ runGithubM envGithubCacheRef envManager (toGithubAuth $ githubAuthToken envFlags) m

liftGithubMAuth :: AuthToken -> GithubM a -> Inspector a
liftGithubMAuth auth m = do
  Environment{..} <- ask
  lift $ withExceptT githubError
       $ runGithubM envGithubCacheRef envManager (toGithubAuth auth) m

githubError :: GH.Error -> ServantErr
githubError e = err500 { errBody = encode $ object
                          ["errors" .= [ "Failure when querying the GitHub API." :: String
                                       , show e
                                       ]]}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Inspection.API.Types
  ( Environment(..)
  , AppError(..)
  , Inspector
  , inspectorToEither
  , liftGithubM
  , liftGithubMAuth
  ) where

import Prelude ()
import MyLittlePrelude

import Control.Monad.Reader       (ReaderT (), ask)
import Control.Monad.Trans.Either (EitherT ())
import Control.Monad.Except       (ExceptT(), withExceptT)
import Control.Monad.Trans        (lift)
import Control.Category

import Data.Data           (toConstr, showConstr)
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

instance Data ServantErr
instance Data GH.Error

data AppError
  = NotAuthorized Text 
  | NotFound Text
  | GithubError GH.Error
  | Other ServantErr
  deriving (Typeable, Data, Generic)

instance Show AppError where
  show (NotAuthorized resource) = "Not authorized " <> show resource <> "."
  show (NotFound resource) = "Not found " <> show resource
  show (GithubError err) = "Failed to query github: " ++ show err
  show (Other err) = show err

instance ToJSON AppError where
  toJSON err = object [ "tag" .= tag
                      , "message" .= show err
                      ]
    where
      tag = showConstr $ toConstr err

toHTTPCode :: AppError -> Int
toHTTPCode NotAuthorized{} = 403
toHTTPCode NotFound{}      = 404
toHTTPCode GithubError{}   = 500
toHTTPCode Other{}         = 500

toServantErr :: AppError -> ServantErr
toServantErr err =
  ServantErr
    { errHTTPCode = toHTTPCode err
    , errReasonPhrase = show err
    , errBody = encode $ object [ "error" .= err ]
    , errHeaders = []
    }

type Inspector = ReaderT Environment (ExceptT AppError IO)

inspectorToEither :: Environment -> Inspector :~> ExceptT ServantErr IO
inspectorToEither env = Nat (withExceptT toServantErr) <<< runReaderTNat env

liftGithubM :: GithubM a -> Inspector a
liftGithubM m = do
  Environment{..} <- ask
  lift $ withExceptT GithubError
       $ runGithubM envGithubCacheRef envManager (toGithubAuth $ githubAuthToken envFlags) m

liftGithubMAuth :: AuthToken -> GithubM a -> Inspector a
liftGithubMAuth auth m = do
  Environment{..} <- ask
  lift $ withExceptT GithubError
       $ runGithubM envGithubCacheRef envManager (toGithubAuth auth) m

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Inspection.GithubM where

import Control.Monad.IO.Class

import Control.Monad.Identity
import Control.Monad.Operational

import Network.HTTP.Client (Manager)

import qualified GitHub as GH

import Inspection.AuthToken

type GithubT m = ProgramT (GH.Request 'True) m
type GithubM = GithubT Identity

runGithubM :: Manager -> AuthToken -> GithubM a -> IO (Either GH.Error a)
runGithubM mgr token@(GH.OAuth . runAuthToken -> auth) m = case view m of
  Return a -> pure (Right a)
  req :>>= k -> do
    GH.executeRequestWithMgr mgr auth req >>= \case
      Right b -> runGithubM mgr token (k b)
      Left err -> pure (Left err)

runGithubT :: forall a m. (Monad m, MonadIO m)
           => Manager -> AuthToken -> GithubT m a -> m (Either GH.Error a)
runGithubT mgr token@(GH.OAuth . runAuthToken -> auth) = viewT >=> eval
  where
    eval :: ProgramViewT (GH.Request 'True) m a -> m (Either GH.Error a)
    eval (Return a) = pure (Right a)
    eval (req :>>= k) = do
        liftIO (GH.executeRequestWithMgr mgr auth req) >>= \case
          Right b -> runGithubT mgr token (k b)
          Left err -> pure (Left err)

githubRequest :: forall a m. GH.Request 'True a -> GithubT m a
githubRequest = singleton

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inspection.GithubM where

import Prelude ()
import MyLittlePrelude

import Control.Monad.Catch (MonadCatch(..), try)

import Control.Monad.Operational

import Control.Monad.Writer.Strict
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)

import qualified Data.Vector as Vector
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Network.URI (uriToString)

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import Data.IORef

import qualified GitHub as GH

data GithubOp a where
  PerformRequest :: GH.Request 'True a -> GithubOp a
  ThrowError :: GH.Error -> GithubOp a
  CatchError :: Program GithubOp a -> (GH.Error -> Program GithubOp a) -> GithubOp a

newtype GithubM a
  = GithubM { unGithubM :: Program GithubOp a }
  deriving (Functor)

instance Applicative GithubM where
  pure = GithubM . pure
  (GithubM mf) <*> (GithubM ma) = GithubM (mf <*> ma)

instance Monad GithubM where
  (GithubM ma) >>= f = GithubM (unGithubM . f =<< ma)

instance MonadError GH.Error GithubM where
  throwError = GithubM . singleton . ThrowError
  catchError (GithubM m) handler = GithubM $ singleton $ CatchError m (unGithubM . handler)

catchNotFound :: a -> GithubM a -> GithubM a
catchNotFound a m =
  m `catchError`
    \case GH.HTTPError (HTTP.StatusCodeException (Status 404 _) _ _) -> pure a
          err -> throwError err

catchUnauthorized :: a -> GithubM a -> GithubM a
catchUnauthorized a m =
  m `catchError`
    \case GH.HTTPError (HTTP.StatusCodeException (Status 401 _) _ _) -> pure a
          err -> throwError err

catchForbidden :: a -> GithubM a -> GithubM a
catchForbidden a m =
  m `catchError`
    \case GH.HTTPError (HTTP.StatusCodeException (Status 403 _) _ _) -> pure a
          err -> throwError err

instance Hashable GH.Auth

type ResourceURL = String

newtype GithubCache
  = GithubCache
      (HashMap (GH.Auth, ResourceURL)
               (HTTP.Response BS.ByteString))

instance Monoid GithubCache where
  mempty = GithubCache mempty
  mappend (GithubCache a) (GithubCache b) = GithubCache (mappend a b)

executeCachedRequest
  :: forall k a m. (MonadIO m, MonadCatch m, MonadWriter GithubCache m)
  => HTTP.Manager -> GH.Auth -> GithubCache -> GH.Request k a -> m (Either GH.Error a)
executeCachedRequest mgr auth ghCache ghReq = do
  liftIO $ print ghReq
  case ghReq of
    GH.Query {} -> runExceptT $
      GH.makeHttpRequest (Just auth) ghReq
        >>= cachedHttpLbs ghCache
        >>= GH.parseResponse

    GH.PagedQuery _ _ l -> runExceptT $
      GH.makeHttpRequest (Just auth) ghReq
        >>= GH.performPagedRequest
               (cachedHttpLbs ghCache)
               (maybe (const True) (\l' -> (< l') . Vector.length) l)
    _ -> liftIO (GH.executeRequestWithMgr mgr auth ghReq)

  where
    cachedHttpLbs
      :: GithubCache -> HTTP.Request -> ExceptT GH.Error m (HTTP.Response LBS.ByteString)
    cachedHttpLbs (GithubCache cache) httpReq = do
       liftIO $ putStrLn resourceUrl
       try (liftIO $ HTTP.httpLbs conditionalHttpReq mgr) >>= \case
         Right res -> do
           tell $ GithubCache $ HashMap.singleton (auth, resourceUrl) (LBS.toStrict <$> res)
           pure res
         Left err@(HTTP.StatusCodeException Status {statusCode = 304} _ _) ->
           maybe (throwError $ GH.HTTPError err)
                 (pure . fmap LBS.fromStrict)
                 cachedResponse

         Left err -> throwError $ GH.HTTPError err 
            
      where
        resourceUrl = uriToString id (HTTP.getUri httpReq) ""

        conditionalHttpReq = httpReq { HTTP.requestHeaders
                                        = cacheHeaders <> HTTP.requestHeaders httpReq }
        
        cachedResponse :: Maybe (HTTP.Response BS.ByteString)
        cachedResponse = HashMap.lookup (auth, resourceUrl) cache
        
        cacheHeaders :: RequestHeaders
        cacheHeaders =
          maybe []
            (\(HTTP.responseHeaders -> responseHeaders) -> catMaybes
              [ (,) hIfModifiedSince <$> lookup hLastModified responseHeaders
              , (,) hIfNoneMatch     <$> lookup hETag responseHeaders ])
            cachedResponse

runGithubM
  :: forall a m. (MonadIO m, MonadCatch m, MonadError GH.Error m)
  => IORef GithubCache -> HTTP.Manager -> GH.Auth -> GithubM a
  -> m a
runGithubM cacheRef mgr token (GithubM m) = eval $ view m
  where
   eval = \case
    Return a  -> pure a
    ThrowError err :>>= _ -> throwError err
    CatchError m' handler :>>= k -> do
      b <- runGithubM cacheRef mgr token (GithubM m')
             `catchError` (runGithubM cacheRef mgr token . GithubM . handler)
      runGithubM cacheRef mgr token $ GithubM $ k b
    PerformRequest req :>>= k -> do
      cache <- liftIO $ readIORef cacheRef
      (eb, cache') <- runWriterT (executeCachedRequest mgr token cache req)
      case eb of
        Left err -> throwError err
        Right b -> do
          liftIO $ modifyIORef' cacheRef (cache' <>)
          runGithubM cacheRef mgr token (GithubM $ k b)

runGithubM'
  :: forall a m. (MonadIO m, MonadCatch m, MonadError GH.Error m)
  => GH.Auth -> GithubM a -> m a
runGithubM' auth m = do
  manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  cacheRef <- liftIO $ newIORef mempty
  runGithubM cacheRef manager auth m

githubRequest :: forall a. GH.Request 'True a -> GithubM a
githubRequest = GithubM . singleton . PerformRequest

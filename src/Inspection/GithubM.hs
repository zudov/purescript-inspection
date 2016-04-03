{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Inspection.GithubM where

import Prelude ()
import MyLittlePrelude

import Control.Monad.Catch (MonadCatch(..), try)

import Control.Monad.Identity
import Control.Monad.Operational

import Control.Monad.Writer.Strict
import Control.Monad.Except (MonadError(..))

import qualified Data.Vector as Vector
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Network.URI (uriToString)

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import qualified Network.HTTP.Client as HTTP

import Data.IORef

import qualified GitHub as GH

type GithubT m = ProgramT (GH.Request 'True) m
type GithubM = GithubT Identity

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
  :: forall k a m. (MonadIO m, MonadCatch m, MonadError GH.Error m, MonadWriter GithubCache m)
  => HTTP.Manager -> GH.Auth -> GithubCache -> GH.Request k a -> m a
executeCachedRequest mgr auth ghCache ghReq = do
  liftIO $ print ghReq
  case ghReq of
    GH.Query {} ->
      GH.makeHttpRequest (Just auth) ghReq
        >>= cachedHttpLbs ghCache
        >>= GH.parseResponse

    GH.PagedQuery _ _ l ->
      GH.makeHttpRequest (Just auth) ghReq
        >>= GH.performPagedRequest
               (cachedHttpLbs ghCache)
               (maybe (const True) (\l' -> (< l') . Vector.length) l)
    _ -> either throwError pure =<< liftIO (GH.executeRequestWithMgr mgr auth ghReq)

  where
    cachedHttpLbs
      :: GithubCache -> HTTP.Request -> m (HTTP.Response LBS.ByteString)
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
runGithubM cacheRef mgr token m =
  case view m of
    Return a   -> pure a
    req :>>= k -> do
      cache <- liftIO $ readIORef cacheRef
      (b, cache') <- runWriterT (executeCachedRequest mgr token cache req)
      liftIO $ modifyIORef' cacheRef (cache' <>)
      runGithubM cacheRef mgr token (k b)

-- runGithubT :: forall a m. (Monad m, MonadIO m)
--            => IORef GithubCache -> HTTP.Manager -> AuthToken -> GithubT m a
--            -> ExceptT GH.Error m a
-- runGithubT cacheRef mgr token = viewT >=> eval
--   where
--     eval :: ProgramViewT (GH.Request 'True) (ExceptT GH.Error m) a -> ExceptT GH.Error m a
--     eval (Return a) = pure a
--     eval (req :>>= k) = do
--       cache <- liftIO $ readIORef cacheRef
--       undefined
--     --  liftIO (executeCachedRequest mgr token cache req) >>= \case
--     --    Right (cache', b) -> runGithubT cache' mgr token (k b)
--     --    Left err -> pure (Left err)

githubRequest :: forall a m. GH.Request 'True a -> GithubT m a
githubRequest = singleton

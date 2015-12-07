{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
module Inspection.API.Sync
  ( SyncAPI
  , syncServer
  ) where

import Control.Monad.Reader       (ask, lift, liftIO)
import Control.Monad.Trans.Either (left)
import Data.List                  (sort)
import Data.Maybe                 (fromMaybe)

import Data.Acid
import Servant

import Inspection.API.Types
import Inspection.AuthToken
import Inspection.BuildConfig
import Inspection.BuildMatrix
import Inspection.Config
import Inspection.Database
import Inspection.Flags
import Inspection.PackageName
import Inspection.ReleaseTag

type SyncAPI =
  Header "Authorization" AuthToken :> (
       "compiler"
         :> Capture "compiler" Compiler
            :> QueryParam "limit" Int
               :> Get '[] ()
  :<|> "package"
         :> Capture "packageName" PackageName
            :> QueryParam "limit" Int
               :> Get '[] ()
  )

syncServer :: ServerT SyncAPI Inspector
syncServer mAuthToken = syncCompiler mAuthToken
                   :<|> syncPackage mAuthToken

syncCompiler :: Maybe AuthToken -> Compiler -> Maybe Int -> Inspector ()
syncCompiler mUserToken compiler (fromMaybe 5 -> limit) = do
  Environment{..} <- ask
  let token = fromMaybe (githubAuthToken envFlags) mUserToken
  liftIO $ do
    buildConfigs <- fmap (take limit . reverse . sort)
                         (getBuildConfigs envManager token compiler)
    matrix <- query envAcid GetBuildMatrix
    let matrix' = foldr addBuildConfig matrix buildConfigs
    update envAcid (AppendBuildMatrix matrix')

syncPackage :: Maybe AuthToken -> PackageName -> Maybe Int -> Inspector ()
syncPackage mUserToken packageName (fromMaybe 5 -> limit) = do
  Environment{..} <- ask
  let token = fromMaybe (githubAuthToken envFlags) mUserToken
  case packageLocation packageName envConfig of
    Nothing -> lift (left err404)
    Just location -> liftIO $ do
      tags <- fmap (take limit . reverse . sort)
                   (getReleaseTags envManager token location)
      matrix <- query envAcid GetBuildMatrix
      let matrix' = foldr (addReleaseTag packageName) matrix tags
      update envAcid (AppendBuildMatrix matrix')

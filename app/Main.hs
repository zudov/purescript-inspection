{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Prelude ()
import MyLittlePrelude

import Control.Exception (finally)

import Data.Acid                            (closeAcidState, openLocalState)
import Data.IORef
import Network.HTTP.Client                  (newManager)
import Network.HTTP.Client.TLS              (tlsManagerSettings)
import Network.Wai                          (Application)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors,
                                             simpleCorsResourcePolicy,
                                             simpleHeaders, simpleMethods)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant                              (Server, enter, serve)

import           Inspection.API
import           Inspection.API.Types

import qualified Inspection.Config      as Config
import           Inspection.Database
import           Inspection.Flags
import           Inspection.GithubM
import qualified Inspection.BuildLogStorage as BuildLogStorage

newEnvironment :: IO Environment
newEnvironment = do
  envFlags   <- getEnvironmentFlags
  envConfig  <- Config.getConfig "inspection.yaml"
  envManager <- newManager tlsManagerSettings
  envAcid    <- openLocalState initialDB
  envGithubCacheRef <- newIORef (GithubCache mempty)
  envBuildLogStorageEnv <- BuildLogStorage.Environment envManager
                             <$> BuildLogStorage.loadConfig
  pure Environment{..}

main :: IO ()
main = do
  env <- newEnvironment
  finally
    (run 8080 (app env))
    (closeAcidState (envAcid env))

server :: Environment -> Server InspectorAPI
server env = enter (inspectorToEither env) inspectorServer

app :: Environment -> Application
app env = logStdoutDev $ cors (const (Just corsPolicy))
                       $ serve (Proxy :: Proxy InspectorAPI)
                               (server env)
  where
    corsPolicy = simpleCorsResourcePolicy
                  { corsMethods        = simpleMethods <> ["DELETE", "PUT", "PATCH"]
                  , corsRequestHeaders = simpleHeaders <> ["content-type"]
                  }

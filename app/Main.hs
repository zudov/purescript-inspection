{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Exception (finally)
import Data.Monoid       ((<>))

import Data.Acid                            (closeAcidState, createCheckpoint,
                                             openLocalState, update)
import Network.HTTP.Client                  (newManager)
import Network.HTTP.Client.TLS              (tlsManagerSettings)
import Network.Wai                          (Application)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors,
                                             simpleCorsResourcePolicy,
                                             simpleHeaders, simpleMethods)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import           Inspection.API
import           Inspection.API.Types
import           Inspection.BuildMatrix
import qualified Inspection.Config      as Config
import           Inspection.Database
import           Inspection.Flags

newEnvironment :: IO Environment
newEnvironment = do
  envFlags   <- getEnvironmentFlags
  envConfig  <- Config.getConfig "inspection.yaml"
  envManager <- newManager tlsManagerSettings
  envAcid    <- openLocalState initialDB
  pure Environment{..}

main :: IO ()
main = do
  env <- newEnvironment
  finally
    (do createCheckpoint (envAcid env)
        update (envAcid env) (AppendBuildMatrix
                               (populatedBuildMatrix $ Config.packageNames
                                                     $ envConfig env))
        run 8080 (app env))
    (closeAcidState (envAcid env))

server :: Environment -> Server InspectorAPI
server env = enter (inspectorToEither env) inspectorServer

app :: Environment -> Application
app env = logStdoutDev $ cors (const (Just corsPolicy))
                       $ serve (Proxy :: Proxy (InspectorAPI :<|> Raw))
                               (server env :<|> serveDirectory "frontend/dist/")
  where
    corsPolicy = simpleCorsResourcePolicy
                  { corsMethods        = simpleMethods <> ["DELETE", "PUT", "PATCH"]
                  , corsRequestHeaders = simpleHeaders <> ["content-type"]
                  }

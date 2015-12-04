{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Acid                            (closeAcidState, openLocalState,
                                             update, createCheckpoint)
import Network.HTTP.Client                  (newManager)
import Network.HTTP.Client.TLS              (tlsManagerSettings)
import Network.Wai                          (Application)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.Cors          (cors, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import           Inspection.API
import           Inspection.API.Types
import           Inspection.BuildMatrix
import qualified Inspection.Config      as Config
import           Inspection.Database
import           Inspection.Flags

newEnvironment :: IO Environment
newEnvironment = Environment <$> openLocalState initialDB
                             <*> newManager tlsManagerSettings
                             <*> getEnvironmentFlags
                             <*> Config.getConfig "inspection.yaml"

main :: IO ()
main = do
  env <- newEnvironment
  createCheckpoint (envAcid env)

  update (envAcid env) (AppendBuildMatrix
                         (populatedBuildMatrix $ Config.packageNames
                                               $ envConfig env))
  run 8080 (app env)

  closeAcidState (envAcid env)

server :: Environment -> Server InspectorAPI
server env = enter (inspectorToEither env) inspectorServer

app :: Environment -> Application
app env = logStdoutDev $ cors (const (Just corsPolicy))
                       $ serve (Proxy :: Proxy (InspectorAPI :<|> Raw))
                               (server env :<|> serveDirectory "frontend/dist/")
  where
    corsPolicy = simpleCorsResourcePolicy

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Acid                            (closeAcidState, openLocalState,
                                             update)
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

main :: IO ()
main = do
  config <- Config.getConfig "inspection.yaml"
  populated <- populatedBuildMatrix (Config.packages config)
                                    (Config.compilers config)
  acid <- openLocalState initialDB
  update acid (AppendBuildMatrix populated)

  run 8080 (app (Environment acid))

  closeAcidState acid

server :: Environment -> Server InspectorAPI
server env = enter (inspectorToEither env) inspectorServer

app :: Environment -> Application
app env = logStdoutDev $ cors (const (Just corsPolicy))
                       $ serve (Proxy :: Proxy (InspectorAPI :<|> Raw))
                               (server env :<|> serveDirectory "frontend/dist/")
  where
    corsPolicy = simpleCorsResourcePolicy

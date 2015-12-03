{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Acid
import Servant
import Servant.Utils.StaticFiles
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors

import Inspection.Database
import qualified Inspection.Config   as Config
import Inspection.API
import Inspection.API.Types
import Inspection.BuildMatrix

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

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

import Purescript.Inspection.Database
import qualified Purescript.Inspection.Config   as Config
import Purescript.Inspection.API
import Purescript.Inspection.BuildMatrix

main :: IO ()
main = do
  acid <- openLocalState initialDB
  config <- Config.getConfig "inspection.yaml"
  populated <- populatedBuildMatrix (Config.packages config)
                                    (Config.compilers config)
  update acid (AppendBuildMatrix populated)

  run 8080 (app acid)

  closeAcidState acid

server :: AcidState DB -> Server InspectorAPI
server acid = enter (inspectorToEither acid) inspectorServer

app :: AcidState DB -> Application
app acid = logStdoutDev $ cors (const (Just corsPolicy))
                        $ serve (Proxy :: Proxy (InspectorAPI :<|> Raw))
                                (server acid :<|> serveDirectory "frontend/dist/")
  where
    corsPolicy = simpleCorsResourcePolicy

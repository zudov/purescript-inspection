module Main where

import Data.Acid
import Servant
import Network.Wai
import Network.Wai.Handler.Warp

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
app acid = serve (Proxy :: Proxy InspectorAPI) (server acid)

module DevelMain where

import Control.Concurrent
import Data.Acid                            (closeAcidState, openLocalState,
                                             update, createCheckpoint)
import Data.IORef
import Foreign.Store
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


import Main

develMain :: IO ()
develMain = do
  env <- newEnvironment
  createCheckpoint (envAcid env)

  update (envAcid env) (AppendBuildMatrix
                         (populatedBuildMatrix $ Config.packageNames
                                               $ envConfig env))

  appRef <- newIORef (app env)

  tid <- forkIO $ run 8080 (app env)

  writeStore (Store 1) appRef
  writeStore (Store 0) env



stop tid = do
  killThread tid
  env <- readStore env_store
  closeAcidState (envAcid env)

reload = do
  appRef <- readStore appref_store
  env <- readStore env_store
  writeIORef appRef (app env)

appref_store = Store 0
env_store = Store 1


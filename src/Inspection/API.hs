{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Inspection.API where

import Servant

import Inspection.API.BuildMatrix
import Inspection.API.Tasks
import Inspection.API.Types
import Inspection.API.Sync

type InspectorAPI =
       "matrix" :> BuildMatrixAPI
  :<|> "tasks"  :> TasksAPI
  :<|> "sync"   :> SyncAPI

inspectorServer :: ServerT InspectorAPI Inspector
inspectorServer = buildMatrixServer
             :<|> tasksServer
             :<|> syncServer

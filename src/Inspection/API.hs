{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Inspection.API where

import Servant

import Inspection.API.BuildMatrix
import Inspection.API.Tasks
import Inspection.API.Types

type InspectorAPI =
       "matrix" :> BuildMatrixAPI
  :<|> "tasks"  :> TasksAPI

inspectorServer :: ServerT InspectorAPI Inspector
inspectorServer = buildMatrixServer
             :<|> tasksServer

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Inspection.API where

import Servant

import Inspection.API.Types
import Inspection.API.BuildMatrix
import Inspection.API.Tasks

type InspectorAPI =
       "matrix" :> BuildMatrixAPI
  :<|> "tasks"  :> TasksAPI

inspectorServer :: ServerT InspectorAPI Inspector
inspectorServer = buildMatrixServer
             :<|> tasksServer

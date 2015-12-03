{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Purescript.Inspection.API where

import Servant

import Purescript.Inspection.API.Types
import Purescript.Inspection.API.BuildMatrix
import Purescript.Inspection.API.Tasks

type InspectorAPI =
       "matrix" :> BuildMatrixAPI
  :<|> "tasks"  :> TasksAPI

inspectorServer :: ServerT InspectorAPI Inspector
inspectorServer = buildMatrixServer
             :<|> tasksServer

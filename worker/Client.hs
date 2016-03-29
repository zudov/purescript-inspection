{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Client
  where

import Data.Proxy (Proxy(..))
import Control.Monad.Except (ExceptT())

import Network.HTTP.Client (Manager)

import Servant.Client (client, BaseUrl(..), Scheme(..), ServantError)
import Servant.API ((:>)) 

import Inspection.API.Tasks
import Inspection.Data
import Inspection.Data.TaskQueue
import Inspection.EventLog (EventId)
import Inspection.API.BuildMatrix (AddBuildResultBody, AddBuildResultAPI)


host :: BaseUrl
host = BaseUrl Http "localhost" 8080 ""

getTasks :: Manager
         -> Maybe Compiler 
         -> Maybe (ReleaseTag Compiler)
         -> Maybe PackageName
         -> Maybe (ReleaseTag Package)
         -> Bool
         -> ExceptT ServantError IO TaskQueue
getTasks = client (Proxy :: Proxy ("tasks" :> TasksAPI)) host

addBuildResult
  :: Manager
  -> Maybe AuthToken -> PackageName -> ReleaseTag Package -> Compiler -> ReleaseTag Compiler -> AddBuildResultBody
  -> ExceptT ServantError IO EventId
addBuildResult = client (Proxy :: Proxy ("matrix" :> AddBuildResultAPI)) host

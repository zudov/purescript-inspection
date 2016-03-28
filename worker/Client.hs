{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Client
  where

import Data.Proxy (Proxy(..))
import Control.Monad.Trans.Either (EitherT)

import Servant.Client (client, BaseUrl(..), Scheme(..), ServantError)
import Servant.API ((:>)) 

import Inspection.API.Tasks
import Inspection.Data
import Inspection.Data.TaskQueue
import Inspection.EventLog (EventId)
import Inspection.API.BuildMatrix (AddBuildResultBody, AddBuildResultAPI)


host :: BaseUrl
host = BaseUrl Http "localhost" 8080

getTasks :: Maybe Compiler 
         -> Maybe (ReleaseTag Compiler)
         -> Maybe PackageName
         -> Maybe (ReleaseTag Package)
         -> Bool
         -> EitherT ServantError IO TaskQueue
getTasks = client (Proxy :: Proxy ("tasks" :> TasksAPI)) host

addBuildResult
  :: Maybe AuthToken -> PackageName -> ReleaseTag Package -> Compiler -> ReleaseTag Compiler -> AddBuildResultBody
  -> EitherT ServantError IO EventId
addBuildResult = client (Proxy :: Proxy ("matrix" :> AddBuildResultAPI)) host

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Client
  where

import Data.Proxy (Proxy(..))
import Control.Monad.Trans.Either (EitherT)

import Servant.Client (client, BaseUrl(..), Scheme(..), ServantError)
import Servant.API ((:>)) 

import Inspection.API.Tasks
import Inspection.API.BuildMatrix
import Inspection.TaskQueue
import Inspection.ReleaseTag
import Inspection.BuildResult
import Inspection.PackageName
import Inspection.BuildConfig

host :: BaseUrl
host = BaseUrl Http "localhost" 8080

getTasks :: Maybe Compiler 
         -> Maybe ReleaseTag
         -> Maybe PackageName
         -> Maybe ReleaseTag
         -> Bool
         -> EitherT ServantError IO TaskQueue
getTasks = client (Proxy :: Proxy ("tasks" :> TasksAPI)) host

addBuildResult :: PackageName -> ReleaseTag -> Compiler -> ReleaseTag -> BuildResult
               -> EitherT ServantError IO [BuildResult]
addBuildResult = client (Proxy :: Proxy ("matrix" :> AddBuildResultAPI)) host

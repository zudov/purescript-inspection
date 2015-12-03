{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Purescript.Inspection.API.Tasks
  ( TasksAPI
  , tasksServer
  ) where

import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Acid

import Servant

import Purescript.Inspection.TaskQueue
import Purescript.Inspection.ReleaseTag
import Purescript.Inspection.Database
import Purescript.Inspection.BuildConfig
import Purescript.Inspection.PackageName

import Purescript.Inspection.API.Types

type TasksAPI =
  QueryParam "compiler" Compiler
    :> QueryParam "compilerVersion" ReleaseTag
       :> QueryParam "packageName" PackageName
          :> QueryParam "packageVersion" ReleaseTag
            :> QueryFlag "rebuild"
               :> Get '[JSON] TaskQueue

tasksServer :: ServerT TasksAPI Inspector
tasksServer = getQueue

getQueue :: Maybe Compiler -> Maybe ReleaseTag -> Maybe PackageName -> Maybe ReleaseTag
         -> Bool -> Inspector TaskQueue
getQueue mCompiler mCompilerVersion mPackageName mPackageVersion rebuild = do
  acid <- asks envAcid
  matrix <- liftIO (query acid GetBuildMatrix)
  let newTasks = selectTasks mCompiler mCompilerVersion mPackageName mPackageVersion
               $ allYourTasks rebuild
               $ matrix
  pure newTasks 


module Purescript.Inspection.TaskQueue
  ( TaskQueue()
  , emptyTaskQueue
  , addTask
  ) where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Maybe
import Control.Arrow

import Purescript.Inspection.Task

newtype TaskQueue = TaskQueue (Bimap TaskId Task)

emptyTaskQueue :: TaskQueue
emptyTaskQueue = TaskQueue Bimap.empty

addTask :: Task -> TaskQueue -> (TaskId, TaskQueue)
addTask task (TaskQueue queue)
  | Bimap.null queue = (TaskId 0, TaskQueue (Bimap.singleton (TaskId 0) task))
addTask task (TaskQueue queue) =
  (fromJust . Bimap.lookupR task &&& TaskQueue) queue'
  where
    TaskId maxId = fst $ Bimap.findMax queue
    queue' = Bimap.tryInsert (TaskId (maxId + 1)) task queue

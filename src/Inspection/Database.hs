{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Inspection.Database
  ( DB(..)
  , initialDB
  , GetBuildMatrix(..)
  , GetEventLog(..)
  , AddEventRecord(..)
  ) where

import           Control.Monad.Reader (asks)
import           Control.Monad.State  (modify, state)


import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)


import Inspection.BuildMatrix



import Inspection.Event (Event)
import Inspection.EventLog (EventLog, EventRecord(..), EventId(..))
import qualified Inspection.EventLog as EventLog

data DB = DB { buildMatrix :: BuildMatrix
             , eventLog :: EventLog Event
             } deriving (Show, Eq, Generic, Typeable)

initialDB :: DB
initialDB = DB { buildMatrix = mempty
               , eventLog = EventLog.empty
               }

deriveSafeCopy 0 'base ''DB

addEventRecord :: EventRecord Event -> Update DB EventId
addEventRecord eventRecord =
  state $ \db ->
    ( EventId $ length $ eventLog db
    , db { buildMatrix = execute (eventBody eventRecord) (buildMatrix db)
         , eventLog = EventLog.add eventRecord (eventLog db)
         }
    )

getEventLog :: Query DB (EventLog Event)
getEventLog = asks eventLog

getBuildMatrix :: Query DB BuildMatrix
getBuildMatrix = asks buildMatrix

makeAcidic ''DB [ 'getBuildMatrix
                , 'addEventRecord
                , 'getEventLog
                ]

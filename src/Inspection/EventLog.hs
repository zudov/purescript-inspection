{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Inspection.EventLog
  ( EventLog()
  , EventRecord(..)
  , empty
  , add
  , index
  ) where

import Safe (atMay)

import Data.Time.Clock (UTCTime)

import Data.SafeCopy

-- FIXME: Use more performant data structure. Particularly we would need O(1) cons and random access,
--        0(1) concat would also be nice
newtype EventLog a
  = EventLog [EventRecord a]
  deriving (Show, Eq)

instance Functor EventLog where
  fmap f (EventLog records) = EventLog $ fmap (fmap f) $ reverse records

instance Foldable EventLog where
  foldMap f (EventLog records) = foldMap (foldMap f) $ reverse records

data EventRecord a
  = EventRecord
      { commitTime :: UTCTime
      , eventBody  :: a
      }
  deriving (Functor, Foldable, Show, Eq)

deriveSafeCopy 0 'base ''EventRecord
deriveSafeCopy 0 'base ''EventLog

empty :: EventLog a
empty = EventLog []

add :: EventRecord a -> EventLog a -> EventLog a
add record (EventLog eventLog) = EventLog (record : eventLog)

newtype EventId = EventId Int deriving (Eq, Ord, Show)

index :: EventId -> EventLog a -> Maybe (EventRecord a)
index (EventId n) (EventLog eventLog) = atMay (reverse eventLog) n

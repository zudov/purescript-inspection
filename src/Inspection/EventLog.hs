{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Inspection.EventLog
  ( EventLog()
  , EventRecord(..)
  , empty
  , cons
  , index
  ) where

import Safe (atMay)

import Data.Time.Clock (UTCTime)

import Data.SafeCopy

-- FIXME: Use more performant data structure. Particularly we would need O(1) cons and random access,
--        0(1) concat would also be nice
newtype EventLog a
  = EventLog [EventRecord a]
  deriving (Functor, Foldable, Show, Eq, Monoid)


data EventRecord a
  = EventRecord
      { commitTime :: UTCTime
      , eventBody  :: a
      }
  deriving (Functor, Foldable, Show, Eq)

deriveSafeCopy 0 'base ''EventRecord

empty :: EventLog a
empty = EventLog []

cons :: EventRecord a -> EventLog a -> EventLog a
cons record (EventLog eventLog) = EventLog (record : eventLog)

index :: Int -> EventLog a -> Maybe (EventRecord a)
index n (EventLog eventLog) = atMay (reverse eventLog) n

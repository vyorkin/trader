{-# LANGUAGE TemplateHaskell #-}

module Sandbox.CQRS.EventStore
  ( EventStore(..)
  , newEventStore
  , uuid
  , reader
  , writer
  ) where

import Prelude hiding (reader, atomically)

import Control.Lens (makeLenses)
import Eventful
import Eventful.Store.Memory (eventMapTVar, tvarEventStoreReader,
                              tvarEventStoreWriter)

data EventStore e = EventStore
  { _uuid   :: UUID
  , _reader :: VersionedEventStoreReader STM e
  , _writer :: EventStoreWriter STM e
  }

makeLenses ''EventStore

newEventStore :: IO (EventStore e)
newEventStore = do
  u <- uuidNextRandom
  v <- eventMapTVar
  let r = tvarEventStoreReader v
      w = tvarEventStoreWriter v
  return $ EventStore u r w

{-# LANGUAGE ScopedTypeVariables #-}

module Sandbox.Eventful1 where

import Prelude hiding (atomically, reader)

import Control.Lens ((^.))
import Control.Concurrent.STM
import Control.Monad.STM

import Eventful
import Eventful.Store.Memory (getEvents, storeEvents)

import Sandbox.CQRS.EventStore

newtype Counter = Counter { unCounter :: Int }
  deriving (Show, Eq)

incrementCounter :: Counter -> Int -> Counter
incrementCounter (Counter count) amount = Counter (count + amount)

decrementCounter :: Counter -> Int -> Counter
decrementCounter (Counter count) amount = Counter (count - amount)

resetCounter :: Counter -> Counter
resetCounter _ = Counter 0

data CounterEvent
  = CounterIncremented Int
  | CounterDecremented Int
  | CounterReset
  deriving (Show, Eq)

counterProjection :: Projection Counter CounterEvent
counterProjection = Projection
  { projectionSeed = Counter 0
  , projectionEventHandler = handleCounterEvent
  }

handleCounterEvent :: Counter -> CounterEvent -> Counter
handleCounterEvent (Counter count) (CounterIncremented amount) = Counter $ count + amount
handleCounterEvent (Counter count) (CounterDecremented amount) = Counter $ count - amount
handleCounterEvent _ CounterReset = Counter 0

myEvents :: [CounterEvent]
myEvents =
  [ CounterIncremented 3
  , CounterDecremented 1
  , CounterReset
  ]

myLatestCounter :: Counter
myLatestCounter = latestProjection counterProjection myEvents

allMyCounters :: [Counter]
allMyCounters = allProjections counterProjection myEvents

type CounterEventStore = EventStore CounterEvent

counterStoreExample :: IO ()
counterStoreExample = do
  store <- newEventStore
  void $ atomically $ storeEvents (store ^. writer) AnyVersion (store ^. uuid) myEvents
  events' <- atomically $ getEvents (store ^. reader) (allEvents (store ^. uuid))
  print events'

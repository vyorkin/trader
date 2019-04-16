module Sandbox.Eventful2 where

import Prelude hiding (atomically, reader)

import qualified Data.Text as Text
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Lens ((^.))
import Eventful
import Eventful.Store.Memory

import Sandbox.CQRS.EventStore (EventStore(..), newEventStore, uuid, reader, writer)

newtype CounterState = CounterState { unCounterState :: Int }
  deriving (Show, Eq)

data CounterEvent
  = CounterAmountAdded Int
  | CounterOutOfBounds Int
  deriving (Show, Eq)

type CounterProjection = Projection CounterState CounterEvent

counterProjection :: CounterProjection
counterProjection = Projection (CounterState 0) handleCounterEvent

handleCounterEvent :: CounterState -> CounterEvent -> CounterState
handleCounterEvent (CounterState n) (CounterAmountAdded k) = CounterState (n + k)
handleCounterEvent s (CounterOutOfBounds _) = s

data CounterCommand
  = IncrementCounter Int
  | DecrementCounter Int
  | ResetCounter
  deriving (Show, Read, Eq)

type CounterAggregate = Aggregate CounterState CounterEvent CounterCommand

type CounterEventStore = EventStore CounterEvent

handleCounterCommand :: CounterState -> CounterCommand -> [CounterEvent]
handleCounterCommand (CounterState n) (IncrementCounter k) =
  if n + k <= 100
  then [CounterAmountAdded k]
  else [CounterOutOfBounds (n + k)]
handleCounterCommand (CounterState n) (DecrementCounter k) =
  if n - k >= 0
  then [CounterAmountAdded (-k)]
  else [CounterOutOfBounds (n - k)]
handleCounterCommand (CounterState n) ResetCounter =
  [CounterAmountAdded (-n)]

counterAggregate :: CounterAggregate
counterAggregate = Aggregate handleCounterCommand counterProjection

main :: IO ()
main = do
  store <- newEventStore
  forever $ repl store

repl :: CounterEventStore -> IO ()
repl store = do
  let versioned = versionedStreamProjection (store ^. uuid) counterProjection
  latest <- atomically $ getLatestStreamProjection (store ^. reader) versioned
  let currentState = streamProjectionState latest
  putStrLn $ "Current state: " ++ show currentState
  putStrLn "Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):"
  input <- getLine
  case readMaybe (Text.unpack input) of
    Nothing -> putStrLn "Unknown command"
    Just command -> do
      let events = aggregateCommandHandler counterAggregate currentState command
      putStrLn $ "Events generated: " ++ show events
      void . atomically $ storeEvents (store ^. writer) AnyVersion (store ^. uuid) events
  putStrLn ""

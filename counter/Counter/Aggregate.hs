{-# LANGUAGE LambdaCase #-}

module Counter.Aggregate
  ( CounterAggregate
  , handleCommand
  , aggregate
  ) where

import Eventful (Aggregate (..))

import Counter.Command (CounterCommand (..))
import Counter.Event (CounterEvent (..))
import Counter.Projection (Counter (..), projection)

type CounterAggregate = Aggregate Counter CounterEvent CounterCommand

aggregate :: CounterAggregate
aggregate = Aggregate handleCommand projection

handleCommand :: Counter -> CounterCommand -> [CounterEvent]
handleCommand (Counter n) = \case
  Increment k -> [Incremented k]
  Decrement k -> [Decremented k]
  Reset       -> [Incremented (-n)]

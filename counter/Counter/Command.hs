module Counter.Command
  ( CounterCommand(..)
  ) where

data CounterCommand
  = Increment Int
  | Decrement Int
  | Reset
  deriving (Eq, Show, Read)

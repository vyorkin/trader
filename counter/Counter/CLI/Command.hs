module Counter.CLI.Command
  ( Command(..)
  ) where

data Command
  = Increment Int
  | Decrement Int
  | View
  deriving (Eq, Show)

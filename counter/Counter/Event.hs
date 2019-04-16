{-# LANGUAGE TemplateHaskell #-}

module Counter.Event
  ( CounterEvent(..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)

data CounterEvent
  = Incremented Int
  | Decremented Int
  deriving (Eq, Show)

deriveJSON defaultOptions ''CounterEvent

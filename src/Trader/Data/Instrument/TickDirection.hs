{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Instrument.TickDirection
  ( TickDirection(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)

data TickDirection
  = ZeroMinusTick
  | MinusTick
  | PlusTick
  | ZeroPlusTick
  deriving (Generic, Eq, Show)

instance ToJSON TickDirection
instance FromJSON TickDirection

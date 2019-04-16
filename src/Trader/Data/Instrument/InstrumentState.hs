{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Instrument.InstrumentState
  ( InstrumentState(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)

data InstrumentState
  = Open
  | Settled
  | Unlisted
  deriving (Generic, Eq, Show)

instance FromJSON InstrumentState
instance ToJSON InstrumentState

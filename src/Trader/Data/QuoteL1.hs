{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Trader.Data.QuoteL1
  ( QuoteL1(..)
  , symbol
  , askPrice
  , askSize
  , bidPrice
  , bidSize
  , timestamp
  ) where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)

import Trader.Data.Instrument (InstrumentSymbol)

-- | Represents a Level-1 market quote.
data QuoteL1 = QuoteL1
  { _symbol    :: !InstrumentSymbol
  , _askPrice  :: !Double
  , _askSize   :: !Int
  , _bidPrice  :: !Double
  , _bidSize   :: !Int
  , _timestamp :: !UTCTime
  } deriving (Eq, Show, Generic)

makeLenses ''QuoteL1

instance FromJSON QuoteL1
instance ToJSON QuoteL1

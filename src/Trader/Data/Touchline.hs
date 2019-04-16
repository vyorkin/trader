{-# LANGUAGE RecordWildCards #-}

module Trader.Data.Touchline
  ( Touchline(..)
  , isValid
  , spread
  ) where

import Prelude hiding (ask)

-- | Touchline is the highest price that a buyer is willing to pay and
-- the lowest price at which a seller is willing to sell.
-- <https://www.investopedia.com/terms/t/touchline.asp>
data Touchline = Touchline
  { bidPrice :: !Double
  , askPrice :: !Double
  } deriving (Eq, Show)

instance ToText Touchline where
  toText Touchline{..} =
    "ask: "
    <> show askPrice
    <> ", bid: "
    <> show bidPrice

-- | Gets the spread.
-- <https://www.investopedia.com/terms/b/bid-askspread.asp>
spread :: Touchline -> Double
spread Touchline{..} = bidPrice - askPrice

isValid :: Touchline -> Bool
isValid = (>= 0) . spread

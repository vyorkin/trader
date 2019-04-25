{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Trader.Settings
  ( Settings(..)
  ) where

import Dhall (Interpret, Natural)

-- | Trading settings.
data Settings = Settings
  { instrument :: !String  -- ^ Instrument symbol to trade.
  , orderPairs :: !Natural -- ^ How many pairs of buy/sell orders to keep open.
  , orderSize  :: !Natural -- ^ How large each order should be.
  , interval   :: !Double  -- ^ Distance between successive orders, as a percentage (example: 0.025 for 2.5%).
  } deriving (Generic, Show)

instance Interpret Settings

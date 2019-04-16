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
  , interval   :: !Double  -- ^ Distance between successive orders.
  } deriving (Generic, Show)

instance Interpret Settings

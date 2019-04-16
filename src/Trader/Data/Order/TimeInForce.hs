{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Order.TimeInForce
  ( TimeInForce(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default, def)

-- | Specifies how long the order remains in effect.
-- Defaults to `GoodTillCancel`.
-- <https://testnet.bitmex.com/api/explorer/#!/Order/Order_new bitmex api docs>
data TimeInForce
  = Day
  | GoodTillCancel
  | ImmediateOrCancel
  | FillOrKill
  | MarketWithLeftOverAsLimit
  deriving (Generic, Eq, Show)

instance FromJSON TimeInForce
instance ToJSON TimeInForce

instance Default TimeInForce where
  def = GoodTillCancel

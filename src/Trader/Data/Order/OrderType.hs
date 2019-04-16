{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Order.OrderType
  ( OrderType(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default, def)

-- | Order type.
-- Defaults to `Limit`.
-- <https://testnet.bitmex.com/api/explorer/#!/Order/Order_new bitmex api docs>
data OrderType
  -- | Traditional market order. A Market order will execute until
  -- filled or your bankruptcy price is reached, at which point it will cancel.
  = Market
  -- | Limit order (default order type).
  | Limit
  -- | Stop market order.
  | Stop
  -- | Like a `Stop` (market), but enters a `Limit` order instead of a `Market` order.
  | StopLimit
  -- | Similar to a `Stop`, but triggers are done in the opposite direction.
  -- Useful for "take profit" orders.
  | MarketIfTouched
  -- | Same as a `MarketIfTouched`, but for "take porfit limit" orders.
  | LimitIfTouched
  | Pegged
  deriving (Eq, Show, Generic)

instance FromJSON OrderType
instance ToJSON OrderType

instance Default OrderType where
  def = Limit

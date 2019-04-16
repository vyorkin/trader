{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Order.ExecutionInstruction
  ( ExecutionInstruction(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default, def)

-- | Instructions for order handling on exchange trading floor.
-- Defaults to `MarkPrice`.
-- <https://testnet.bitmex.com/api/explorer/#!/Order/Order_new bitmex api docs>
data ExecutionInstruction
    -- |  Also known as a Post-Only order.
    -- If this order would have executed on placement, it will cancel instead.
    = ParticipateDoNotInitiate
    | AllOrNone
    | MarkPrice
    | IndexPrice
    | LastPrice
    -- | Will cancel other active limit orders with the same side and
    -- symbol if the open quantity exceeds the current position.
    -- `Close` implies `ReduceOnly`.
    | Close
    -- | Can only reduce position, not increase it.
    | ReduceOnly
    | Fixed
    deriving (Eq, Show, Generic)

instance FromJSON ExecutionInstruction
instance ToJSON ExecutionInstruction

instance Default ExecutionInstruction where
  def = MarkPrice

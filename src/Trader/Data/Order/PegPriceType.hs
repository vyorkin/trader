{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Order.PegPriceType
  ( PegPriceType(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)

-- | Peg price type.
-- May be used to create trailing stops.
-- <https://testnet.bitmex.com/api/explorer/#!/Order/Order_new bitmex api docs>
data PegPriceType
  = LastPeg
  | MidPricePeg
  | MarketPeg
  | PrimaryPeg
  | TrailingStopPeg
  deriving (Generic, Eq, Show)

instance FromJSON PegPriceType
instance ToJSON PegPriceType

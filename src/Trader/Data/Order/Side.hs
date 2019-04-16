{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Order.Side
  ( Side(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default, def)

-- | Order side. Defaults to `Buy`.
data Side = Buy | Sell
  deriving (Generic, Eq, Show)

instance FromJSON Side
instance ToJSON Side

instance Default Side where
  def = Buy

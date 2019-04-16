{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Order.OrderStatus
  ( OrderStatus(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default, def)

-- | Order status. Defaults to `New`.
data OrderStatus = New | Filled | Canceled
  deriving (Generic, Eq, Show)

instance FromJSON OrderStatus
instance ToJSON OrderStatus

instance Default OrderStatus where
  def = New

{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Currency
  ( Currency(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)

data Currency
  = USD
  | U1
  | XBt
  | KRW
  deriving (Generic, Eq, Show)

instance FromJSON Currency
instance ToJSON Currency

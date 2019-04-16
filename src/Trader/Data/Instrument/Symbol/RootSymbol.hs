{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Instrument.Symbol.RootSymbol
  ( RootSymbol(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default, def)

-- | Root symbol.
data RootSymbol
  = XBT
  | ETH
  | LTC
  | XRP
  deriving (Eq, Read, Show, Generic)

instance ToJSON RootSymbol
instance FromJSON RootSymbol

instance Default RootSymbol where
  def = XBT

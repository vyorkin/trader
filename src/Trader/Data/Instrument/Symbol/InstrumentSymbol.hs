{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Trader.Data.Instrument.Symbol.InstrumentSymbol
  ( InstrumentSymbol(..)
  , mkInstrumentSymbol
  , root
  , currency
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default, def)

import Trader.Data.Currency (Currency (..))
import Trader.Data.Instrument.Symbol.RootSymbol (RootSymbol (..))

-- | Instrument symbol.
data InstrumentSymbol
  = XBTUSD
  | XBTM18
  | XBTU18
  | XBT7D_U110
  | ADAM18
  | BCHM18
  | ETHUSD
  | ETHM18
  | LTCM18
  | XRPM18
  | XBTKRW
  deriving (Eq, Show, Read, Generic)

instance ToJSON InstrumentSymbol
instance FromJSON InstrumentSymbol

instance Default InstrumentSymbol where
  def = XBTUSD

mkInstrumentSymbol :: RootSymbol -> Currency -> Maybe InstrumentSymbol
mkInstrumentSymbol XBT USD = Just XBTUSD
mkInstrumentSymbol ETH USD = Just ETHUSD
mkInstrumentSymbol _ _     = Nothing

root :: InstrumentSymbol -> Maybe RootSymbol
root = \case
  XBTUSD -> Just XBT
  ETHUSD -> Just ETH
  _      -> Nothing

currency :: InstrumentSymbol -> Maybe Currency
currency = \case
  XBTUSD -> Just USD
  ETHUSD -> Just USD
  _      -> Nothing

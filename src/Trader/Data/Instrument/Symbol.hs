{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Trader.Data.Instrument.Symbol
  ( Symbol(..)
  , module Trader.Data.Instrument.Symbol.RootSymbol
  , module Trader.Data.Instrument.Symbol.InstrumentSymbol
  ) where

import Data.Aeson (ToJSON, toJSON)
import Prelude hiding (show)
import Text.Show (Show, show)

import Trader.Data.Instrument.Symbol.InstrumentSymbol (InstrumentSymbol (..),
                                                       currency,
                                                       mkInstrumentSymbol,
                                                       rootSymbol)
import Trader.Data.Instrument.Symbol.RootSymbol (RootSymbol (..))

data Symbol
  = Root RootSymbol
  | Instrument InstrumentSymbol
  deriving (Eq)

instance Show Symbol where
  show = \case
    Root s -> show s
    Instrument s -> show s

instance ToJSON Symbol where
  toJSON = \case
    Root s -> toJSON s
    Instrument s -> toJSON s

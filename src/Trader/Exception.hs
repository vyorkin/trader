{-# LANGUAGE RecordWildCards #-}

module Trader.Exception
  ( SettingsException(..)
  , ExchangeException(..)
  ) where

import Prelude hiding (show, ask)
import Text.Show (show)
import Control.Lens ((^.))
import qualified Data.Text as Text

import Trader.Data.PriceLevel (PriceLevel(..))
import Trader.Data.Touchline (Touchline(..))

data SettingsException
  = InvalidInstrumentException String

instance Exception SettingsException
instance Show SettingsException where
  show (InvalidInstrumentException sym) =
    "Invalid instrument in settings: " <> sym

data ExchangeException
  = InvalidTouchline !Touchline

instance Exception ExchangeException
instance Show ExchangeException where
  show (InvalidTouchline touchline) =
    Text.unpack $ unlines
      [ "Invalid touchline:"
      , toText touchline
      ]

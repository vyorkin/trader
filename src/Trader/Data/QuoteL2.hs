{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Trader.Data.QuoteL2
  ( QuoteL2(..)
  , side
  , size
  , price
  , isBuy
  , isSell
  ) where

import Control.Lens (makeLenses, (^.))
import Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)

import Trader.Data.Order.Side (Side (..))

-- | Represents a Level-2 market quote.
data QuoteL2 = QuoteL2
  { _side  :: !Side
  , _size  :: !Int
  , _price :: !Double
  } deriving (Eq, Show, Generic)

makeLenses ''QuoteL2

instance Ord QuoteL2 where
  compare l r = (l ^. price) `compare` (r ^. price)

instance ToText QuoteL2 where
  toText QuoteL2{..} = show _side
    <> " " <> show _price
    <> " (" <> show _size <> ")"

deriveJSON (defaultOptions { fieldLabelModifier = drop 1 }) ''QuoteL2

isBuy :: QuoteL2 -> Bool
isBuy QuoteL2{..} = _side == Buy

isSell :: QuoteL2 -> Bool
isSell = not . isBuy

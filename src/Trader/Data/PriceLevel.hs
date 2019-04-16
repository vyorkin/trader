{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Trader.Data.PriceLevel
  ( PriceLevel(..)
  , mkPriceLevel
  , touchline
  ) where

import Control.Lens (makeLenses, (^.))
import qualified Data.List as List
import Prelude hiding (ask)
import qualified Relude.Unsafe as Unsafe

import Trader.Data.QuoteL2 (QuoteL2, isSell, price)
import Trader.Data.Touchline (Touchline(..))

-- | Level-2 market quotes.
-- <https://www.investopedia.com/terms/q/quoted-price.asp>
data PriceLevel = PriceLevel
  { ask :: ![QuoteL2]
  , bid :: ![QuoteL2]
  } deriving (Eq, Show)

makeLenses ''PriceLevel

instance ToText PriceLevel where
  toText PriceLevel{..} = unlines $
    [ "price level:"
    , "ASK"
    ] <> (toText <$> ask) <>
    [ "BID"
    ] <> (toText <$> bid)

-- | Makes a new `PriceLevel` given the given list of quotes.
mkPriceLevel :: [QuoteL2] -> PriceLevel
mkPriceLevel quotes =
  let (bid, ask) = List.partition isSell quotes
  in PriceLevel{..}

-- | Gets the touchline.
touchline :: PriceLevel -> Touchline
touchline PriceLevel{..} =
  let
    bidPrice = Unsafe.head bid ^. price
    askPrice = Unsafe.head ask ^. price
  in Touchline{..}

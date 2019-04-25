module Trader.Capability.Resource.QuoteL2
  ( QuoteL2Resource
  , listQuoteL2
  ) where

import Trader.Capability.Http (Http)
import Trader.Data.QuoteL2 (QuoteL2)
import Trader.Data.Instrument.Symbol (RootSymbol)

class Http m => QuoteL2Resource m where
  listQuoteL2 :: RootSymbol -> Int -> m [QuoteL2]

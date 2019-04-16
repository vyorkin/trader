{-# LANGUAGE FlexibleContexts #-}

module Trader.API.Endpoint.QuoteL2
  ( list
  ) where

import Network.HTTP.Req (GET (..), NoReqBody (..), (/:), (=:))
import qualified Network.HTTP.Req as Req

import Trader.API.Request (request)
import Trader.App (MonadApp)
import Trader.Data.Instrument.Symbol (RootSymbol)
import Trader.Data.QuoteL2 (QuoteL2)

list :: MonadApp m => RootSymbol -> Int -> m [QuoteL2]
list symbol depth = request GET (\u -> u /: "orderBook" /: "L2") query NoReqBody
  where
    query :: Req.Option scheme
    query = "symbol" =: (show symbol :: Text) <> "depth" =: depth

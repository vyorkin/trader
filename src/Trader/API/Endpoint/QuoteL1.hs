{-# LANGUAGE FlexibleContexts #-}

module Trader.API.Endpoint.QuoteL1
  ( list
  ) where

import Network.HTTP.Req (GET (..), NoReqBody (..), (/:))

import Trader.API.Endpoint.Filter (Filter)
import qualified Trader.API.Endpoint.Filter as Filter
import Trader.API.Request (request)
import Trader.App (MonadApp)
import Trader.Data.QuoteL1 (QuoteL1)

list :: MonadApp m => Filter -> m [QuoteL1]
list f = request GET (/: "quote") (Filter.toQuery f) NoReqBody

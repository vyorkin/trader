{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards  #-}

module Trader
  ( init
  , module Trader.App
  , module Trader.Env
  ) where

import Prelude hiding (init)
import Data.Default.Class (def)
import Control.Lens (view)
import Colog (pattern I, log)

import Trader.App (App, runApp, MonadApp)
import Trader.Env (Env(..), touchline, rootSymbol, orders)
import Control.Monad.Catch (MonadThrow, throwM)
import Trader.Data.PriceLevel (mkPriceLevel)
import qualified Trader.Data.PriceLevel as PriceLevel
import Trader.Data.Touchline (Touchline (..), spread)
import qualified Trader.Data.Touchline as Touchline
import qualified Trader.Data.Order as Order (toMap)
import qualified Trader.API.Endpoint.Order as Order
import qualified Trader.API.Endpoint.QuoteL2 as QuoteL2
import Trader.Exception (ExchangeException (..))

init :: MonadApp m => m ()
init = do
  log I "cancelling all orders"
  Order.cancelAll

  log I "caching level-2 quotes"
  quotes <- view rootSymbol >>= flip QuoteL2.list 1
  let
    level = mkPriceLevel quotes
    line@Touchline{..} = PriceLevel.touchline level
  validateTouchline line
  view touchline >>= flip putMVar line

  log I $ "touchline: " <> toText line
  log I $ "spread: " <> show (spread line)

  log I "caching orders"
  pendingOrders <- Order.listNew def
  traverse_ (log I) (toText <$> pendingOrders)
  let orderMap = Order.toMap pendingOrders
  view orders >>= flip putMVar orderMap

-- | Performs a simple sanity check for the `Touchline`.
validateTouchline :: MonadThrow m => Touchline -> m ()
validateTouchline line = when (not $ Touchline.isValid line) $
  throwM $ InvalidTouchline line

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Trader.Loop
  ( run
  ) where

import Colog (LogAction, pattern D, pattern I, log, withLogTextFile, (*<), (<&), (>$), (>$<), (>*), (>*<), (>|<))
import Control.Concurrent (modifyMVar_, threadDelay)
import Control.Lens (view, over, (%~), (.~), (^.))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Prelude hiding (ask, log, reverse, (>$<), (>$))
import qualified Data.Map.Strict as Map
import qualified Relude.Unsafe as Unsafe
import GHC.Natural (naturalToInt)

import Control.Monad.Throw.Extra (maybeThrow)
import Trader (MonadApp)
import qualified Trader.API.Endpoint.Order as Order
import Trader.API.Endpoint.Filter (Filter(..))
import Trader.API.Endpoint.Order.Placement (Placement (..), mkLimit)
import qualified Trader.API.Endpoint.QuoteL2 as QuoteL2
import Trader.Config (Config, settings)
import Trader.Data.Instrument.Symbol (InstrumentSymbol (..), RootSymbol (..),
                                      rootSymbol)
import Trader.Data.Order (Order(..))
import qualified Trader.Data.Order as Order
import qualified Trader.Data.Order.OrderStatus as OrderStatus
import Trader.Data.Order.Side (Side (..))
import Trader.Data.PriceLevel (PriceLevel (..), mkPriceLevel)
import qualified Trader.Data.PriceLevel as PriceLevel
import qualified Trader.Data.Touchline as Touchline
import Trader.Data.QuoteL2 (QuoteL2 (..))
import Trader.Data.Touchline (Touchline (..), spread)
import Trader.Env (config, orders, priceLevel)
import Trader.Exception (SettingsException (..), ExchangeException(..))
import Trader.Settings (Settings (..))
import Trader.Util.JSON (pretty, prettyPrint)

run :: MonadApp m => m ()
run = do
  -- log I "cancelling all orders"
  -- Order.cancelAll
  delayMs 200
  loop

loop :: MonadApp m => m ()
loop = do
  cfg <- view config
  let Settings{..} = cfg ^. settings
  (isym, sym) <- maybeThrow (InvalidInstrumentException instrument) $ do
    inst <- readMaybe instrument
    root <- rootSymbol inst
    return (inst, root)

  log I "caching level-2 quotes"
  quotes <- QuoteL2.list sym 1
  let
    level = mkPriceLevel quotes
    touchline@Touchline{..} = PriceLevel.touchline level

  log I $ "touchline: " <> toText touchline
  log I $ "spread: " <> show (spread touchline)

  validateTouchline touchline
  view priceLevel >>= flip putMVar level

  log I "caching orders"
  pendingOrders <- Order.listNew def
  traverse_ (log I) (toText <$> pendingOrders)
  let orderMap = Order.toMap pendingOrders
  view orders >>= flip putMVar orderMap

  -- TODO: check for already placed orders
  -- TODO: close not-profitable positions

  log I "placing orders"
  for_ [0..(naturalToInt orderPairs)] $ \i -> do
    delayMs 200
    let delta = interval * fromIntegral i
    let qty = naturalToInt orderSize
    sell <- Order.place $ mkLimit Sell isym (bidPrice + delta) qty
    delayMs 200
    buy  <- Order.place $ mkLimit Buy isym (askPrice - delta) qty
    traverse_ (log I) (toText <$> [sell, buy])

  delayMs 500

  -- loop

-- | Performs a simple sanity check for the `Touchline`.
validateTouchline :: MonadThrow m => Touchline -> m ()
validateTouchline touchline =
  when (not $ Touchline.isValid touchline) $
    throwM $ InvalidTouchline touchline

-- | Suspends the current thread for a given number of milliseconds.
delayMs :: MonadApp m => Int -> m ()
delayMs ms = liftIO $ threadDelay $ 1000 * ms

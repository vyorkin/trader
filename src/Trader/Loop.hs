{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Trader.Loop
  ( run
  ) where

import Colog (pattern D, pattern I, LogAction, log, withLogTextFile, (*<), (<&),
              (>$), (>$<), (>*), (>*<), (>|<))
import Control.Concurrent (forkIO, modifyMVar_, threadDelay)
import Control.Lens (over, view, (%~), (.~), (^.))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import GHC.Natural (naturalToInt)
import Prelude hiding (init, ask, log, reverse, (>$), (>$<))
import qualified Relude.Unsafe as Unsafe

import Control.Monad.Throw.Extra (maybeThrow)
import Trader (MonadApp)
import Trader.API.Endpoint.Filter (Filter (..))
import qualified Trader.API.Endpoint.Order as Order
import Trader.API.Endpoint.Order.Placement (Placement (..), mkLimit)
import qualified Trader.API.Endpoint.QuoteL2 as QuoteL2
import Trader.Config (Config, settings)
import Trader.Data.Instrument.Symbol (InstrumentSymbol (..), RootSymbol (..))
import Trader.Data.Order (Order (..))
import qualified Trader.Data.Order as Order
import qualified Trader.Data.Order.OrderStatus as OrderStatus
import Trader.Data.Order.Side (Side (..))
import qualified Trader.Data.Order.Side as Side
import Trader.Data.PriceLevel (PriceLevel (..), mkPriceLevel)
import qualified Trader.Data.PriceLevel as PriceLevel
import Trader.Data.QuoteL2 (QuoteL2 (..))
import Trader.Data.Touchline (Touchline (..), spread)
import qualified Trader.Data.Touchline as Touchline
import Trader.Env (config, instrumentSymbol, orders, rootSymbol, touchline)
import Trader.Exception (ExchangeException (..), SettingsException (..))
import Trader.Settings (Settings (..))
import Trader.Util.JSON (pretty, prettyPrint)

run :: MonadApp m => m ()
run = do
  cfg <- view config
  -- TODO: check for already placed orders
  -- TODO: close not-profitable positions

  let pairs = orderPairs $ cfg ^. settings
  log I $ "placing " <> show pairs <> " pairs"
  for_ [1..naturalToInt pairs + 1] $ \i -> do
    sell <- placeLimit Sell i
    buy  <- placeLimit Buy (-i)
    traverse_ (log I) ((<> "<$ ") . toText <$> [sell, buy])

-- | Places a new limit order given a `Side` and index.
placeLimit :: MonadApp m => Side -> Int -> m Order
placeLimit side index = do
  delayOrder
  cfg <- view config
  sym <- view instrumentSymbol
  let Settings{..} = cfg ^. settings
  line <- view touchline >>= readMVar
  let
    delta = 1.0 + interval
    basePrice = Touchline.price line side
    realPrice = basePrice * (delta ** fromIntegral index)
    price = fromIntegral (round realPrice :: Int)
    count = naturalToInt orderSize
    placement = mkLimit side sym price count
  log I $ "$> " <> toText placement
  Order.place placement

-- | Suspends the current thread for a given number of milliseconds.
delayMs :: MonadApp m => Int -> m ()
delayMs ms = liftIO $ threadDelay $ 1000 * ms

-- | Delays order placement.
-- Prevents account to be marked as a SPAM.
delayOrder :: MonadApp m => m ()
delayOrder = delayMs $ 5 * 100

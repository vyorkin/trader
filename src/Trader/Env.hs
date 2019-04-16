{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Trader.Env
  ( Env(..)
  , config
  , network
  , logger
  , priceLevel
  , orders
  , newEnv
  ) where

import Colog (HasLog, LogAction, Message, cmapM, defaultFieldMap,
              fmtRichMessageDefault, getLogAction, liftLogIO, logTextStdout,
              setLogAction, upgradeMessageAction)
import Control.Concurrent (MVar)
import Control.Lens (makeLenses)

import Trader.Config (Config)
import Trader.Data.Network (Network)
import Trader.Data.Order (Order, OrderId)
import Trader.Data.PriceLevel (PriceLevel)

-- | Global environment stores read-only information and
-- mutable refs to global state available to any
-- function with the `MonadReader` constraint.
data Env m = Env
  { -- | Application config.
    _config :: !Config
    -- | Exchange network to trade on.
  , _network :: !Network
    -- | A `LogAction` to be used by the `co-log` package.
  , _logger :: !(LogAction m Message)
    -- | Level-2 market quotes.
  , _priceLevel :: !(MVar PriceLevel)
    -- | Current (pending) orders.
  , _orders :: !(MVar (Map OrderId Order))
  }

makeLenses ''Env

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = _logger

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction _logger env = env { _logger }

-- | Creates a record that matches the `Env` type our
-- application requires by filling in necessary fields.
newEnv
  :: MonadIO m
  => Config
  -> Network
  -> LogAction IO Text
  -> IO (Env m)
newEnv _config _network logTextFile = do
  let
    logText = logTextStdout <> logTextFile
    logRich = cmapM fmtRichMessageDefault logText
    logFull = upgradeMessageAction defaultFieldMap logRich
    _logger = liftLogIO logFull
  _priceLevel <- newEmptyMVar
  _orders <- newEmptyMVar
  return Env{..}

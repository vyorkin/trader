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
  , instrumentSymbol
  , rootSymbol
  , touchline
  , orders
  , newEnv
  ) where

import Colog (HasLog, LogAction, Message, cmapM, defaultFieldMap,
              fmtRichMessageDefault, getLogAction, liftLogIO, logTextStdout,
              setLogAction, upgradeMessageAction)
import Control.Concurrent (MVar)
import Control.Lens (makeLenses, (^.))

import Control.Monad.Throw.Extra (maybeThrow)
import Trader.Config (Config, settings)
import Trader.Data.Instrument.Symbol (InstrumentSymbol (..), RootSymbol (..))
import qualified Trader.Data.Instrument.Symbol.InstrumentSymbol as InstrumentSymbol
import Trader.Data.Network (Network)
import Trader.Data.Order (OrderMap)
import Trader.Data.Touchline (Touchline)
import Trader.Exception (SettingsException (..))
import Trader.Settings (Settings (..))

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
    -- | Instrument symbol.
  , _instrumentSymbol :: !InstrumentSymbol
    -- | Root symbol.
  , _rootSymbol :: !RootSymbol
    -- | Level-2 market quotes.
  , _touchline :: !(MVar Touchline)
    -- | Current (pending) orders.
  , _orders :: !(MVar OrderMap)
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
  (_instrumentSymbol, _rootSymbol) <- readSymbols _config
  _touchline <- newEmptyMVar
  _orders <- newEmptyMVar
  return Env{..}

readSymbols :: Config -> IO (InstrumentSymbol, RootSymbol)
readSymbols cfg = do
  let Settings{..} = cfg ^. settings
  maybeThrow (InvalidInstrumentException instrument) $ do
    inst <- readMaybe instrument
    root <- InstrumentSymbol.root inst
    return (inst, root)

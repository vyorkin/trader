{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main
  ( main
  , start
  , server
  ) where

import Prelude hiding (log)
import Colog (pattern I, withLogTextFile, log)
import Options.Applicative (execParser)
import System.Directory (createDirectoryIfMissing)

import Trader (runApp, MonadApp)
import Trader.Env (newEnv)
import Trader.Data (Network)
import qualified Trader.Loop as Loop
import qualified Trader.Data.Network as Network (toName)
import Trader.Config (Config)
import qualified Trader.Config as Config (load)

import CLI (Options (..), network, usage)
import qualified Dotenv as Dotenv (setup)

-- | Server CLI app entry point.
main :: IO ()
main = do
  options <- execParser usage
  let net = network options
  Dotenv.setup $ ".env." <> Network.toName net
  Config.load net >>= \case
    Just cfg -> start cfg net
    Nothing  -> putStrLn "Unable to load config"

-- | Start trading bot server.
start :: Config -> Network -> IO ()
start cfg net = do
  createDirectoryIfMissing True "logs"
  withLogTextFile logFilePath \logger -> do
    env <- newEnv cfg net logger
    runApp env server
  where
    logFilePath = "logs/" <> Network.toName net <> ".log"

server :: MonadApp m => m ()
server = do
  log I "start"
  Loop.run
  log I "stop"

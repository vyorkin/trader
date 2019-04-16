{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Trader.Config
  ( Config(..)
  , load
  , apiKey
  , dbHost
  , dbPort
  , dbName
  , dbUser
  , dbPassword
  , settings
  , mkConnectionString
  ) where

import Control.Lens (makeLenses)
import qualified Data.ByteString.Char8 as Char8 (pack)
import Database.Persist.Postgresql (ConnectionString (..))
import qualified Dhall
import System.Environment (lookupEnv)

import qualified Trader.API as API (Key)
import qualified Trader.API.Auth.Key as APIKey (load)

import Trader.Data.Network (Network)
import qualified Trader.Data.Network as Network
import Trader.Settings (Settings)

-- | Application config.
data Config = Config
  { _apiKey     :: !API.Key        -- ^ API key
  , _dbHost     :: !String         -- ^ DB host
  , _dbPort     :: !Int            -- ^ DB port
  , _dbName     :: !String         -- ^ DB name
  , _dbUser     :: !(Maybe String) -- ^ DB user
  , _dbPassword :: !(Maybe String) -- ^ DB password
  , _settings   :: !Settings       -- ^ Trading settings
  } deriving (Show)

makeLenses ''Config

-- | Load application config.
load :: Network -> IO (Maybe Config)
load net = do
  _dbHost <- "DB_HOST" `withDef` "localhost"
  _dbName <- "DB_NAME" `withDef` ("trader-" <> Network.toName net)
  _dbUser <- lookupEnv "DB_USER"
  _dbPassword <- lookupEnv "DB_PASSWORD"
  _settings <- Dhall.input Dhall.auto "./settings.dhall"
  runMaybeT $ do
    _dbPort <- "DB_PORT" `readDef` 5432
    _apiKey <- MaybeT APIKey.load
    return Config {..}
  where
    withDef k v = liftIO $ fromMaybe v <$> lookupEnv k
    readDef k v = MaybeT $ maybe (pure v) readMaybe <$> lookupEnv k

mkConnectionString :: Config -> ConnectionString
mkConnectionString Config{..} = Char8.pack
   $ "host="     <> _dbHost
  <> "port="     <> show _dbPort
  <> "dbname="   <> show _dbName
  <> "user="     <> fromMaybe "" _dbUser
  <> "password=" <> fromMaybe "" _dbPassword

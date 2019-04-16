{-# LANGUAGE RecordWildCards #-}

module Counter.CLI
  ( app
  ) where

import Configuration.Dotenv (configPath)
import qualified Configuration.Dotenv as Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (NoLoggingT (..), runNoLoggingT)
import qualified Data.ByteString.Char8 as Char8 (pack)
import Database.Persist.Postgresql (createPostgresqlPool, runMigrationSilent,
                                    runSqlPool)
import Eventful (uuidFromInteger)
import Eventful.Store.Postgresql (migrateSqlEvent)
import System.Environment (lookupEnv)

import Counter.CLI.Options (Options (..))
import qualified Counter.CLI.Options as Options (runParser)
import Counter.CLI.Run (run)

app :: IO ()
app = do
  Options{..} <- Options.runParser
  void $ Dotenv.loadFile $ Dotenv.defaultConfig { configPath = [".env.sandbox"] }
  conn <- fromMaybe "fuck" <$> lookupEnv "DB_CONNECTION"
  pool <- runNoLoggingT $ createPostgresqlPool (Char8.pack conn) 2
  let uuid = uuidFromInteger 42
  void $ flip runSqlPool pool $ runMigrationSilent migrateSqlEvent
  liftIO $ run pool uuid command

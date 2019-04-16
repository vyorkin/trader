module Counter.CLI.Store
  ( CounterEventHandler
  , runDb
  , reader
  , writer
  , globalReader
  , prettyPrint
  ) where

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (Indent (..), confIndent, encodePretty')
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, runSqlPool)
import Prelude hiding (reader)

import Eventful (GlobalEventStoreReader, UUID, synchronousEventBusWrapper)
import Eventful.Store.Class (serializedVersionedEventStoreReader)
import Eventful.Store.Postgresql (EventStoreWriter, VersionedEventStoreReader,
                                  defaultSqlEventStoreConfig,
                                  jsonStringSerializer,
                                  postgresqlEventStoreWriter,
                                  serializedEventStoreWriter,
                                  serializedGlobalEventStoreReader,
                                  sqlEventStoreReader,
                                  sqlGlobalEventStoreReader)

import Counter.Model (CounterEvent)

type CounterEventHandler m =
  EventStoreWriter (SqlPersistT m) CounterEvent -> UUID -> CounterEvent -> SqlPersistT m ()

runDb :: ConnectionPool -> SqlPersistT IO a -> IO a
runDb = flip runSqlPool

reader :: MonadIO m => VersionedEventStoreReader (SqlPersistT m) CounterEvent
reader = serializedVersionedEventStoreReader jsonStringSerializer $
  sqlEventStoreReader defaultSqlEventStoreConfig

writer :: MonadIO m => EventStoreWriter (SqlPersistT m) CounterEvent
writer = synchronousEventBusWrapper writer' [eventPrinter]
  where
    store = postgresqlEventStoreWriter defaultSqlEventStoreConfig
    writer' = serializedEventStoreWriter jsonStringSerializer store

globalReader :: MonadIO m => GlobalEventStoreReader (SqlPersistT m) CounterEvent
globalReader = serializedGlobalEventStoreReader
  jsonStringSerializer (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)

eventPrinter :: MonadIO m => CounterEventHandler m
eventPrinter _ uuid event = liftIO $ prettyPrint (uuid, event)

prettyPrint :: ToJSON a => a -> IO ()
prettyPrint = LazyChar8.putStrLn . encodePretty' (Pretty.defConfig { confIndent = Spaces 2 })

{-# LANGUAGE LambdaCase #-}

module Counter.CLI.Run
  ( run
  ) where

import Database.Persist.Postgresql (ConnectionPool)
import Eventful (UUID, getLatestStreamProjection, streamProjectionState,
                 versionedStreamProjection)
import Eventful.Aggregate (commandStoredAggregate)

import Counter.Aggregate (aggregate)
import Counter.CLI.Command (Command)
import qualified Counter.CLI.Command as CLI (Command (..))
import Counter.CLI.Store (prettyPrint, runDb)
import qualified Counter.CLI.Store as Store
import Counter.Command (CounterCommand (..))
import Counter.Projection (projection)

run :: ConnectionPool -> UUID -> Command -> IO ()
run pool uuid = \case
  CLI.Increment n -> handle pool uuid (Increment n)
  CLI.Decrement n -> handle pool uuid (Decrement n)
  CLI.View -> do
    let versionedProj = versionedStreamProjection uuid projection
    proj <- runDb pool $ getLatestStreamProjection Store.reader versionedProj
    prettyPrint $ streamProjectionState proj

handle :: ConnectionPool -> UUID -> CounterCommand -> IO ()
handle pool uuid cmd = do
  putStr "command: "
  print cmd
  void $ runDb pool $ commandStoredAggregate
    Store.writer Store.reader aggregate uuid cmd

module CLI
  ( Options(..)
  , parser
  , usage
  ) where

import Trader.Data (Network (..))
import Options.Applicative

-- | CLI options.
data Options = Options
  { network :: !Network -- ^ Exchange network
  , dryRun  :: !Bool    -- ^ Don't place any orders for real
  } deriving (Show)

-- | CLI options parser.
parser :: Parser Options
parser = Options
  <$> networkParser
  <*> switch (long "dry-run" <> short 'd' <> help "Don't place any orders")

networkParser :: Parser Network
networkParser =
  flag' MainNet
  (  long "main-net"
  <> short 'm'
  <> help "Trade on a real exchange (mainnet)"
  )
  <|>
  flag' TestNet
  (  long "test-net"
  <> short 't'
  <> help "Use a testnet for trading"
  )

-- | Describes what the program does.
-- To be displayed in the help screen.
usage :: ParserInfo Options
usage = info (parser <**> helper)
  (  fullDesc
  <> progDesc "Simple trading bot"
  <> header "trader - money maker"
  )

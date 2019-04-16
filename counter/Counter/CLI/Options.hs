module Counter.CLI.Options
  ( Options(..)
  , runParser
  ) where

import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper,
                            info, long, option, progDesc, short, subparser)
import qualified Options.Applicative as Parser (command)

import Counter.CLI.Command (Command (..))

data Options = Options
  { command :: Command
  } deriving (Show)

runParser :: IO Options
runParser = execParser $ info (helper <*> parser) (fullDesc <> progDesc "counter")

parser :: Parser Options
parser = Options <$> subparser
  (  Parser.command "inc" (info (helper <*> (Increment <$> parseValue)) (progDesc "Increase counter"))
  <> Parser.command "dec" (info (helper <*> (Decrement <$> parseValue)) (progDesc "Decrease counter"))
  <> Parser.command "view" (info (pure View) (progDesc "View counter"))
  )

parseValue :: Parser Int
parseValue = option auto (long "number" <> short 'n' <> help "Value")

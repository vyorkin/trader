module Dotenv (setup) where

import Configuration.Dotenv (loadSafeFile, configPath, defaultValidatorMap, defaultConfig)
import Control.Monad (void)

-- | Load dotenv file, setup environment variables.
setup :: FilePath -> IO ()
setup filePath =
  void $ loadSafeFile defaultValidatorMap ".scheme.yml" config
  where config = defaultConfig { configPath = [filePath] }

module Trader.Util.JSON
  ( prettyPrint
  , pretty
  ) where

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (Indent (..), confIndent, encodePretty')
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as LazyChar8

prettyPrint :: ToJSON a => a -> IO ()
prettyPrint = LazyChar8.putStrLn . pretty

pretty :: ToJSON a => a -> LazyChar8.ByteString
pretty = encodePretty' (Pretty.defConfig { confIndent = Spaces 2 })

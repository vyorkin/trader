module Trader.API.Request.Expiration
  ( -- * The @Expiration@ type
    Expiration(..)
    -- * Operations
  , newExpiration
  , toInt
  , toByteString
  , toHeader
  ) where

import qualified Data.ByteString.Char8 as Char8 (pack)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Network.HTTP.Req (header)
import qualified Network.HTTP.Req as Req

-- | Represents a UNIX timestamp (in seconds) after
-- which the request is no longer valid.
newtype Expiration = Expiration Int
  deriving (Eq, Show)

-- | Creates a new expiration.
newExpiration :: IO Expiration
newExpiration = Expiration . scale . (+ 1000) <$> getPOSIXTime

-- | Converts `Expiration` to `Int`.
toInt :: Expiration -> Int
toInt = coerce

-- | Converts `Expiration` to `ByteString`.
toByteString :: Expiration -> ByteString
toByteString = Char8.pack . show . toInt

-- | Convenience function to generate a timestamp
-- for the signature of the request.
scale :: POSIXTime -> Int
scale = floor . (* 1000000)

-- | Creates a request header from the given `Expiration`.
toHeader :: Expiration -> Req.Option scheme
toHeader = header "api-expires" . show . toInt

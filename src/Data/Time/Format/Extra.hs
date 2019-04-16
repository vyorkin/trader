module Data.Time.Format.Extra
  ( iso8601
  ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- | Converts `UTCTime` to the ISO-8601 format.
-- `yyyy-mm-ddThh:mm:ss[.sss]Z` (ISO 8601:2004(E) sec. 4.3.2 extended format)
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

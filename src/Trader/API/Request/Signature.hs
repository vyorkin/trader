{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Trader.API.Request.Signature
  ( -- * The @SiSignature@ type
    Signature(..)
    -- * Operations
  , mkSignature
  , toByteString
    -- * Helper functions
  , sign
  ) where

import Prelude hiding (exp)

import Control.Lens (views)
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (hmac, hmacGetDigest)
import Data.ByteArray
import qualified Data.ByteString.Base16 as Base16
import qualified Network.HTTP.Types as Http

import Trader.API.Auth.Key (Key)
import qualified Trader.API.Auth.Key as Key (secret)
import Trader.API.Request.Expiration (Expiration)
import qualified Trader.API.Request.Expiration as Expiration (toByteString)

-- | Represents a signature of the request to BitMEX API.
-- <https://www.bitmex.com/app/apiKeysUsage#Authenticating-with-an-API-Key bitmex api docs>
newtype Signature = Signature ByteString
  deriving (Eq, Show)

-- | Creates a request `Signature`.
mkSignature
  :: Key         -- ^ API key
  -> Http.Method -- ^ HTTP method
  -> ByteString  -- ^ URL path
  -> Expiration  -- ^ Expiration (UNIX timestamp)
  -> ByteString  -- ^ Request body
  -> Signature
mkSignature key verb path expiration body =
  sign key (verb <> path <> exp <> body)
  where
    exp :: ByteString
    exp = Expiration.toByteString expiration

-- | Given the API key and payload makes a signature.
sign :: Key -> ByteString -> Signature
sign key
  = Signature
  . Base16.encode
  . convert
  . hmacSha256 secret
  where
    secret :: ByteString
    secret = views Key.secret encodeUtf8 key

-- | Signs the given message using the
-- HMAC SHA256 and returns `Digest SHA256`.
hmacSha256
  :: (ByteArrayAccess secret, ByteArrayAccess message)
  => secret
  -> message
  -> Digest SHA256
hmacSha256 secret = hmacGetDigest . hmac secret

-- | Converts `Signagure` to `ByteString`.
toByteString :: Signature -> ByteString
toByteString = coerce

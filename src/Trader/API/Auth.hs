{-# LANGUAGE TemplateHaskell #-}

module Trader.API.Auth
  ( -- * The @Auth@ type
    Auth(..)
    -- * Operations
  , claim
    -- * Lenses
  , key
  , signature
  , expiration
  , module Trader.API.Auth.Key
  ) where

import Prelude hiding (exp)

import Control.Lens (makeLenses)
import Network.HTTP.Client (Request)
import Network.HTTP.Req (attachHeader)

import Trader.API.Auth.Key (Key)
import qualified Trader.API.Auth.Key as Key
import Trader.API.Request.Expiration (Expiration)
import qualified Trader.API.Request.Expiration as Expiration
import Trader.API.Request.Signature (Signature)
import qualified Trader.API.Request.Signature as Signature

-- | Represents an API authentication data.
-- <https://www.bitmex.com/app/apiKeysUsage bitmex api docs>
data Auth = Auth
  { _key        :: !Key        -- ^ Authentication key.
  , _signature  :: !Signature  -- ^ Request signature.
  , _expiration :: !Expiration -- ^ Expiration timestamp.
  } deriving (Show)

makeLenses ''Auth

-- | Authenticates the `Request`.
claim :: Auth -> Request -> Request
claim (Auth k sig exp)
  = attachHeader "api-key" (Key.toByteString k)
  . attachHeader "api-expires" (Expiration.toByteString exp)
  . attachHeader "api-signature" (Signature.toByteString sig)

{-# LANGUAGE DeriveGeneric #-}
module Trader.API.Request.SignatureTest where

import Prelude hiding (exp)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Time.Clock as Clock
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.HTTP.Req (DELETE (..), GET (..), HEAD (..), HttpMethod (..),
                         PATCH (..), POST (..), PUT (..))
import qualified Network.HTTP.Req as Req

import Trader.API.Auth.Key (Key (..))
import Trader.API.Request.Expiration (Expiration (..), newExpiration)
import qualified Trader.API.Request.Expiration as Expiration
import Trader.API.Request.Signature (Signature (..), mkSignature)
import qualified Trader.API.Request.Signature as Signature

-- These tests exist just to make sure I did everything
-- correctly, according to the BitMEX documentation examples

-- verb = 'GET'
-- path = '/api/v1/instrument'
-- expires = 1518064236 # 2018-02-08T04:30:36Z
-- data = ''

-- c7682d435d0cfe87c16098df34ef2eb5a549d4c5a3c2b1f0f77b8af73423bf00

hprop_signature_ex1 :: Property
hprop_signature_ex1 = property $ do
  let
    path = "/api/v1/instrument"
    exp = Expiration 1518064236
    sig = mkSignature key methodGet path exp mempty
  Signature.toByteString sig === expected
  where
    expected :: ByteString
    expected = "c7682d435d0cfe87c16098df34ef2eb5a549d4c5a3c2b1f0f77b8af73423bf00"

-- verb = 'GET'
-- # Note url-encoding on querystring - this is '/api/v1/instrument?filter={"symbol": "XBTM15"}'
-- # Be sure to HMAC *exactly* what is sent on the wire
-- path = '/api/v1/instrument?filter=%7B%22symbol%22%3A+%22XBTM15%22%7D'
-- expires = 1518064237 # 2018-02-08T04:30:37Z
-- data = ''

-- e2f422547eecb5b3cb29ade2127e21b858b235b386bfa45e1c1756eb3383919f

hprop_signature_ex2 :: Property
hprop_signature_ex2 = property $ do
  let
    path = "/api/v1/instrument?filter=%7B%22symbol%22%3A+%22XBTM15%22%7D"
    exp = Expiration 1518064237
    sig = mkSignature key methodGet path exp mempty
  Signature.toByteString sig === expected
  where
    expected :: ByteString
    expected = "e2f422547eecb5b3cb29ade2127e21b858b235b386bfa45e1c1756eb3383919f"

-- Simplified (for testing purposes)
-- Aeson-serializable data type
-- that represents a request to create an order
data Order = Order
  { symbol   :: !Text
  , price    :: !Float
  , clOrdID  :: !Text
  , orderQty :: !Int
  } deriving (Generic, Show)

instance ToJSON Order
instance FromJSON Order

-- verb = 'POST'
-- path = '/api/v1/order'
-- expires = 1518064238 # 2018-02-08T04:30:38Z
-- data = '{"symbol":"XBTM15","price":219.0,"clOrdID":"mm_bitmex_1a/oemUeQ4CAJZgP3fjHsA","orderQty":98}'
-- data = '{\"orderQty\":98,\"symbol\":\"XBTM15\",\"price\":219,\"clOrdID\":\"mm_bitmex_1a/oemUeQ4CAJZgP3fjHsA\"}'

-- 1749cd2ccae4aa49048ae09f0b95110cee706e0944e6a14ad0b3a8cb45bd336b

hprop_signature_ex3 :: Property
hprop_signature_ex3 = property $ do
  let
    path = "/api/v1/order"
    exp = Expiration 1518064238
    body = toStrict $ Aeson.encode order
    sig = mkSignature key methodPost path exp body
  Signature.toByteString sig === expected
  where
    expected :: ByteString
    expected = "51446faa35c2b8a681f51acfd6495fe712d400bbd2b967b344a1a7cbaecda4e0"

    order :: Order
    order = Order
      { symbol = "XBTM15"
      , price = 219.0
      , clOrdID = "mm_bitmex_1a/oemUeQ4CAJZgP3fjHsA"
      , orderQty = 98
      }

-- API key is taken from here:
-- https://www.bitmex.com/app/apiKeysUsage#Full-sample-calculation
key :: Key
key = Key "LAqUlngMIQkIUjXMUreyu3qn" "chNOOS4KvNXR_Xq4k4c9qsfoKWvnDecLATCRlcBwyKDYnWgO"

{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Trader.Data.Network
  ( Network(..)
  , toName
  , toUrl
  ) where

import Network.HTTP.Req (Scheme (..), Url, https)

-- | An exchange network.
data Network
  = MainNet
  | TestNet
  deriving (Eq, Show)

-- | Get the name of the given `Network`.
toName :: Network -> String
toName MainNet = "mainnet"
toName TestNet = "testnet"

-- | Creates a `Url` from the given `Network`.
toUrl :: Network -> Url 'Https
toUrl = \case
  MainNet -> https "www.bitmex.com"
  TestNet -> https "testnet.bitmex.com"

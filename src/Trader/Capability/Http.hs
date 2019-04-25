module Trader.Capability.Http
  ( Method(..)
  , Http
  , request
  ) where

import Data.Aeson (FromJSON)

data Method
  = Get
  | Post
  | Put
  | Delete

class Monad m => Http m where
  request :: FromJSON a => Method -> Maybe a -> m b

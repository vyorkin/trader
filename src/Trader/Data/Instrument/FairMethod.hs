{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Instrument.FairMethod
  ( FairMethod(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)

data FairMethod
  = ImpactMidPrice
  | FundingRate
  deriving (Generic, Eq, Show)

instance ToJSON FairMethod
instance FromJSON FairMethod

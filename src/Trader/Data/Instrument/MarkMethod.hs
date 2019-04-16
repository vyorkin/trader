{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Instrument.MarkMethod
  ( MarkMethod(..)
  ) where

data MarkMethod
  = FairPrice
  | LastPrice
  | MarkMethod !Text
  deriving (Eq, Show)

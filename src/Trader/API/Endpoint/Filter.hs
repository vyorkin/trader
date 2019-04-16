{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Trader.API.Endpoint.Filter
  ( Filter(..)
  , toQuery
  ) where

import Prelude hiding (reverse)

import Data.Aeson (encode)
import Data.Default.Class (Default)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.Extra (iso8601)
import Network.HTTP.Req ((=:))
import Web.HttpApiData (ToHttpApiData)
import qualified Network.HTTP.Req as Req
import qualified Data.ByteString.Lazy.Char8 as LazyChar8

import Trader.Data.Instrument (Symbol)

type FilterParams = Map String String

data Filter = Filter
  { symbol    :: !(Maybe Symbol)
  , timeframe :: !(Maybe Timeframe)
  , limit     :: !(Maybe Int)
  , offset    :: !(Maybe Int)
  , startTime :: !(Maybe UTCTime)
  , endTime   :: !(Maybe UTCTime)
  , reverse   :: !(Maybe Bool)
  , params    :: !(Maybe FilterParams)
  } deriving (Eq, Show, Generic, Default)

data Timeframe
  = Daily
  | Weekly
  | Monthly
  | Quarterly
  | Biquarterly
  deriving (Eq, Show)

instance ToText Timeframe where
  toText = Text.toLower . show

toQuery :: Filter -> Req.Option scheme
toQuery Filter{..}
   = "symbol"    ?=? mkSym symbol timeframe
  <> "count"     ?=? limit
  <> "start"     ?=? offset
  <> "startTime" ?=? (iso8601 <$> startTime)
  <> "endTime"   ?=? (iso8601 <$> endTime)
  <> "reverse"   ?=? reverse
  <> "filter"    ?=? (LazyChar8.unpack . encode <$> params)
  where
    mkSym :: Maybe Symbol -> Maybe Timeframe -> Maybe Text
    mkSym Nothing _              = Nothing
    mkSym (Just sym) Nothing     = Just $ show sym
    mkSym (Just sym) (Just time) = Just $ show sym <> ":" <> toText time

infix 7 ?=?
(?=?) :: ToHttpApiData a => Text -> Maybe a -> Req.Option scheme
_ ?=? Nothing = mempty
k ?=? Just v = k =: v

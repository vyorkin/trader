{-# LANGUAGE DeriveGeneric #-}

module Trader.Data.Instrument
  ( Instrument(..)
  , InstrumentState(..)
  , TickDirection(..)
  , FairMethod(..)
  , MarkMethod(..)
  , module Trader.Data.Instrument.Symbol
  , module Trader.Data.Instrument.InstrumentState
  , module Trader.Data.Instrument.TickDirection
  , module Trader.Data.Instrument.FairMethod
  , module Trader.Data.Instrument.MarkMethod
  ) where

import Data.Time.Clock (UTCTime)

import Trader.Data.Currency (Currency)
import Trader.Data.Instrument.FairMethod (FairMethod (..))
import Trader.Data.Instrument.InstrumentState (InstrumentState (..))
import Trader.Data.Instrument.MarkMethod (MarkMethod (..))
import Trader.Data.Instrument.Symbol (Symbol, RootSymbol, InstrumentSymbol)
import Trader.Data.Instrument.TickDirection (TickDirection (..))

data Instrument = Instrument
  { symbol :: !Symbol
  , rootSymbol :: !RootSymbol
  , state :: !InstrumentState
  , typ :: !Text
  , listing :: !(Maybe UTCTime)
  , front :: !(Maybe UTCTime)
  , expiry :: !(Maybe UTCTime)
  , settle :: !(Maybe UTCTime)
  , relistInterval :: !(Maybe UTCTime)
  , inverseLeg :: !(Maybe Text)
  , sellLeg :: !(Maybe Text)
  , buyLeg :: !(Maybe Text)
  , optionStrikePcnt :: !(Maybe Int)
  , optionStrikeRound :: !(Maybe Int)
  , optionStrikePrice :: !(Maybe Int)
  , optionMultiplier :: !(Maybe Double)
  , positionCurrency :: !(Maybe Currency)
  , underlying :: !Currency
  , quoteCurrency :: !Currency
  , underlyingSymbol :: !Text
  , reference :: !Text
  , referenceSymbol :: !Text
  , calcInterval :: !(Maybe UTCTime)
  , publishInterval :: !(Maybe UTCTime)
  , publishTime :: !(Maybe UTCTime)
  , maxOrderQty :: !(Maybe Int)
  , maxPrice :: !(Maybe Int)
  , lotSize :: !(Maybe Int)
  , tickSize :: !Double
  , multiplier :: !(Maybe Double)
  , settlCurrency :: !(Maybe Currency)
  , underlyingToPositionMultiplier :: !(Maybe Double)
  , underlyingToSettleMultiplier :: !(Maybe Double)
  , quoteToSettleMultiplier :: !(Maybe Double)
  , isQuanto :: !Bool
  , isInverse :: !Bool
  , initMargin :: !(Maybe Double)
  , maintMargin :: !(Maybe Double)
  , riskLimit :: !(Maybe Int)
  , riskStep :: !(Maybe Int)
  , limit :: !(Maybe Double)
  , capped :: !Bool
  , taxed :: !Bool
  , deleverage :: !Bool
  , makerFee :: !(Maybe Double)
  , takerFee :: !(Maybe Double)
  , settlementFee :: !(Maybe Double)
  , insuranceFee :: !(Maybe Double)
  , fundingBaseSymbol :: !(Maybe Text)
  , fundingQuoteSymbol :: !(Maybe Text)
  , fundingPremiumSymbol :: !(Maybe Text)
  , fundingTimestamp :: !(Maybe UTCTime)
  , fundingInterval :: !(Maybe UTCTime)
  , fundingRate :: !(Maybe Double)
  , indicativeFundingRate :: !(Maybe Double)
  , rebalanceTimestamp :: !(Maybe UTCTime)
  , rebalanceInterval :: !(Maybe UTCTime)
  , openingTimestamp :: !(Maybe UTCTime)
  , closingTimestamp :: !(Maybe UTCTime)
  , sessionInterval :: !(Maybe UTCTime)
  , prevClosePrice :: !(Maybe Double)
  , limitDownPrice :: !(Maybe Double)
  , limitUpPrice :: !(Maybe Double)
  , bankruptLimitDownPrice :: !(Maybe Double)
  , bankruptLimitUpPrice :: !(Maybe Double)
  , prevTotalVolume :: !(Maybe Int)
  , totalVolume :: !(Maybe Int)
  , volume :: !(Maybe Int)
  , volume24h :: !(Maybe Int)
  , prevTotalTurnover :: !(Maybe Int)
  , totalTurnover :: !(Maybe Int)
  , turnover :: !(Maybe Int)
  , turnover24h :: !(Maybe Int)
  , homeNotional24h :: !(Maybe Int)
  , foreignNotional24h :: !(Maybe Int)
  , prevPrice24h :: !(Maybe Double)
  , vwap :: !(Maybe Double)
  , highPrice :: !(Maybe Double)
  , lowPrice :: !(Maybe Double)
  , lastPrice :: !(Maybe Double)
  , lastPriceProtected :: !(Maybe Double)
  , lastTickDirection :: !(Maybe TickDirection)
  , lastChangePcnt :: !(Maybe Double)
  , bidPrice :: !(Maybe Double)
  , midPrice :: !(Maybe Double)
  , askPrice :: !(Maybe Double)
  , impactBidPrice :: !(Maybe Double)
  , impactMidPrice :: !(Maybe Double)
  , impactAskPrice :: !(Maybe Double)
  , hasLiquidity :: !Bool
  , openInterest :: !(Maybe Int)
  , openValue :: !(Maybe Int)
  , fairMethod :: !(Maybe FairMethod)
  , fairBasisRate :: !(Maybe Double)
  , fairBasis :: !(Maybe Double)
  , fairPrice :: !(Maybe Double)
  , markMethod :: !MarkMethod
  , markPrice :: !(Maybe Double)
  , indicativeTaxRate :: !(Maybe Double)
  , indicativeSettlePrice :: !(Maybe Double)
  , optionUnderlyingPrice :: !(Maybe Double)
  , settledPrice :: !(Maybe Double)
  , timestamp :: !(Maybe UTCTime)
  } deriving (Eq)

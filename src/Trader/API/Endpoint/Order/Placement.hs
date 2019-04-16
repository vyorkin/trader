{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Trader.API.Endpoint.Order.Placement
  ( Placement(..)
  , mkLimit
  ) where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Default.Class (Default, def)

import Trader.Data.Instrument (InstrumentSymbol)
import Trader.Data.Order (ClientOrderId)
import Trader.Data.Order.ExecutionInstruction (ExecutionInstruction (..))
import Trader.Data.Order.OrderType (OrderType (..))
import Trader.Data.Order.PegPriceType (PegPriceType (..))
import Trader.Data.Order.Side (Side (..))
import Trader.Data.Order.TimeInForce (TimeInForce (..))

-- | Order placement parameters.
data Placement = Placement
  { -- | Client Order ID. It will come back on the order and any related executions.
    clientOrderId :: !(Maybe ClientOrderId)
    -- | Instrument symbol.
  , symbol :: !InstrumentSymbol
    -- | Order type.
  , orderType :: !OrderType
    -- | Order side.
  , side :: !Side
    -- | Order quantity in units of the instrument (i.e. contracts).
  , orderQty :: !Int
    -- | Limit price for `Limit`, `StopLimit` and `LimitIfTouched` orders.
  , price :: !(Maybe Double)
    -- | Quantity to display in the book.
    -- Set to `0` to make it a fully hidden order.
  , displayQty :: !(Maybe Double)
    -- | Trigger price for `Stop`, `StopLimit`,
    -- `MarketIfTouched`, and `LimitIfTouched` orders
  , stopPx :: !(Maybe Double)
    -- | Trailing offset from the current price.
  , pegOffsetValue :: !(Maybe Double)
    -- | Peg price type.
  , pegPriceType :: !(Maybe PegPriceType)
    -- | Specifies how long the order remains in effect.
    -- Defaults to `GoodTillCancel` for `Limit`,
    -- `StopLimit`, and `LimitIfTouched` orders.
  , timeInForce :: !(Maybe TimeInForce)
    -- | Execution instruction.
  , executionInstruction :: !(Maybe ExecutionInstruction)
    -- | Custom order annotation.
  , annotation :: !(Maybe Text)
  } deriving (Eq, Show, Generic, Default)

instance ToJSON Placement where
  toJSON Placement{..} = object
    [ "symbol" .= symbol
    , "side" .= side
    , "orderQty" .= orderQty
    , "price" .= price
    , "displayQty" .= displayQty
    , "stopPx" .= stopPx
    , "clOrdID" .= clientOrderId
    , "pegOffsetValue" .= pegOffsetValue
    , "pegPriceType" .= pegPriceType
    , "ordType" .= orderType
    , "timeInForce" .= timeInForce
    , "execInst" .= executionInstruction
    , "text" .= annotation
    ]

-- | Creates a limit-order placement parameters.
mkLimit :: Side -> InstrumentSymbol -> Double -> Int -> Placement
mkLimit side symbol price orderQty =
  def { symbol, side, price = Just price, orderQty }

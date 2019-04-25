module Trader.Capability.Resource.Order
  ( OrderResource
  , listOrders
  , placeOrder
  , placeOrders
  , amendOrder
  , amendOrders
  , cancelOrders
  , cancelAllOrders
  ) where

import Trader.Capability.Http (Http)
import Trader.Data.Order (Order (..), OrderId)
import Trader.API.Endpoint.Filter (Filter (..))
import qualified Trader.API.Endpoint.Filter as Filter
import Trader.API.Endpoint.Order.Placement (Placement)

class Http m => OrderResource m where
  listOrders :: Filter -> m [Order]
  placeOrder :: Placement -> m Order
  placeOrders :: [Placement] -> m [Order]
  amendOrder :: Order -> m Order
  amendOrders :: [Order] -> m [Order]
  cancelOrders :: [OrderId] -> m ()
  cancelAllOrders :: m ()

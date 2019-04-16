{-# LANGUAGE FlexibleContexts #-}

module Trader.API.Endpoint.Order
  ( list
  , listNew
  , place
  , placeBulk
  -- , amend
  -- , amendBulk
  , cancel
  , cancelAll
  , module Trader.API.Endpoint.Order.Placement
  ) where

import Network.HTTP.Req (DELETE (..), GET (..), NoReqBody (..), POST (..),
                         PUT (..), ReqBodyJson (..), (/:), (/~))

import Trader.API.Endpoint.Filter (Filter (..))
import qualified Trader.API.Endpoint.Filter as Filter
import Trader.API.Endpoint.Order.Placement (Placement)
import Trader.API.Request (request)
import Trader.App (MonadApp)
import Trader.Data.Order (Order (..), OrderId)
import qualified Trader.Data.Order as Order

list :: MonadApp m => Filter -> m [Order]
list f = request GET (/: "order") (Filter.toQuery f) NoReqBody

listNew :: MonadApp m => Filter -> m [Order]
listNew f = filter Order.isNew <$> list f

place :: MonadApp m => Placement -> m Order
place o = request POST (/: "order") mempty (ReqBodyJson o)

placeBulk :: MonadApp m => [Placement] -> m [Order]
placeBulk os = request POST (\u -> u /: "order" /: "bulk") mempty (ReqBodyJson os)

-- amend :: MonadApp m => Order -> m ()
-- amend o = request PUT (/: "order") mempty (ReqBodyJson o)

-- amendBulk :: MonadApp m => [Order] -> m ()
-- amendBulk os = request PUT (\u -> u /: "order" /: "bulk") mempty (ReqBodyJson os)

cancel :: MonadApp m => [OrderId] -> m ()
cancel ids = request DELETE (/: "order") mempty NoReqBody

cancelAll :: MonadApp m => m ()
cancelAll = request DELETE (\u -> u /: "order" /: "all") mempty NoReqBody

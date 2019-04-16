module Network.HTTP.Req.Extra
  ( (=?)
  ) where

import Network.HTTP.Req (QueryParam, queryParam)
import Web.HttpApiData (ToHttpApiData (..))

infix 7 =?
(=?) :: (QueryParam param, ToHttpApiData a) => Text -> Maybe a -> param
(=?) = queryParam

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Trader.API.Request
  ( request
  , module Trader.API.Request.Signature
  , module Trader.API.Request.Expiration
  ) where

import Prelude hiding (exp, filter, get, put)

import Colog (WithLog, Message, pattern D, log)
import Control.Lens (view, (^.))
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Aeson (FromJSON)
import Network.HTTP.Client (Request, RequestBody (..))
import qualified Network.HTTP.Client as Request
import Network.HTTP.Req (MonadHttp, AllowsBody, HttpBody, HttpBodyAllowed, HttpMethod,
                         ProvidesBody, Scheme (..), Url, customAuth,
                         getRequestBody, httpMethodName, jsonResponse, req,
                         responseBody, (/:))
import qualified Network.HTTP.Req as Req

import Trader.Env (Env)
import Trader.API.Auth (Auth (..))
import qualified Trader.API.Auth as Auth
import Trader.API.Request.Expiration (Expiration, newExpiration)
import Trader.API.Request.Signature (Signature, mkSignature)
import Trader.Config (apiKey)
import qualified Trader.Data.Network as Network
import Trader.Env (config, network)

request
  :: ( WithLog (Env z) Message m
     , MonadUnliftIO m
     , MonadHttp m
     , HttpMethod method
     , FromJSON a
     , HttpBody body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     )
  => method
  -> (Url 'Https -> Url 'Https)
  -> Req.Option 'Https
  -> body
  -> m a
request (verb :: method) mkUrl query body = do
  url <- withUrl mkUrl
  log D $ method <> " -> " <> show url
  res <- withRunInIO $ \io -> do
    let auth = customAuth (io . authenticate (getBody body))
    io $ req verb url body jsonResponse (query <> auth)
  return $ responseBody res
  where
    method = decodeUtf8 $ httpMethodName (Proxy @method)

getBody :: HttpBody body => body -> ByteString
getBody body = case getRequestBody body of
  RequestBodyLBS x -> toStrict x
  _                -> ""

authenticate
  :: ( MonadIO m
     , WithLog (Env z) Message m
     )
  => ByteString -> Request -> m Request
authenticate body r = do
  cfg <- view config
  exp <- liftIO $ newExpiration
  let
    key  = cfg ^. apiKey
    path = Request.path r <> Request.queryString r
    sig  = mkSignature key (Request.method r) path exp body
    auth = Auth key sig exp
  -- log D $ show auth
  return $ Auth.claim auth r

withUrl
  :: MonadReader (Env z) m
  => (Url 'Https -> Url 'Https) -> m (Url 'Https)
withUrl f = do
  url <- Network.toUrl <$> view network
  return $ f (url /: "api" /: "v1")

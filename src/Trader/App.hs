{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Trader.App
  ( App(..)
  , runApp
  , MonadApp
  ) where

import Colog (Message, WithLog, logException)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Natural (type (~>))
import Control.Exception (throwIO)
import Network.HTTP.Req (HttpConfig, defaultHttpConfig, HttpException, MonadHttp, handleHttpException, getHttpConfig)

import Trader.Env (Env(..))

newtype App a = App
  { unApp :: ReaderT (Env App) IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader (Env App)
    , MonadThrow
    )

instance MonadHttp App where
  handleHttpException :: HttpException -> App a
  handleHttpException ex = do
    logException ex
    liftIO $ throwIO ex

  getHttpConfig :: App HttpConfig
  getHttpConfig = return defaultHttpConfig

runApp :: Env App -> App ~> IO
runApp env = usingReaderT env . unApp

-- | Synonym for constraints commonly
-- satisfied by monads used in stack.
type MonadApp m =
  ( WithLog (Env App) Message m
  , MonadIO m
  , MonadUnliftIO m
  , MonadThrow m
  , MonadHttp m
  )

{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.DebitRejected
  ( DebitRejected(..)
  , remainingBalance
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data DebitRejected = DebitRejected
  { _remainingBalance :: !Double
  } deriving (Eq, Show)

makeLenses ''DebitRejected
deriveJSON defaultOptions ''DebitRejected

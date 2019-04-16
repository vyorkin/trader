{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.TransferStarted
  ( TransferStarted(..)
  , transferId
  , amount
  , targetAccount
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data TransferStarted = TransferStarted
  { _transferId :: !UUID
  , _amount :: !Double
  , _targetAccount :: !UUID
  } deriving (Eq, Show)

makeLenses ''TransferStarted
deriveJSON defaultOptions ''TransferStarted

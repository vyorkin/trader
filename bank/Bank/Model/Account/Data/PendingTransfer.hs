{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Data.PendingTransfer
  ( PendingTransfer(..)
  , transferId
  , amount
  , targetAccount
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data PendingTransfer = PendingTransfer
  { _transferId :: !UUID
  , _amount :: !Double
  , _targetAccount :: !UUID
  } deriving (Eq, Show)

makeLenses ''PendingTransfer
deriveJSON defaultOptions ''PendingTransfer

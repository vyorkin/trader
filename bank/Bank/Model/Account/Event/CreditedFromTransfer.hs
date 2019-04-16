{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.CreditedFromTransfer
  ( CreditedFromTransfer(..)
  , transferId
  , sourceAccount
  , amount
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data CreditedFromTransfer = CreditedFromTransfer
  { _transferId :: !UUID
  , _sourceAccount :: !UUID
  , _amount :: !Double
  } deriving (Eq, Show)

makeLenses ''CreditedFromTransfer
deriveJSON defaultOptions ''CreditedFromTransfer

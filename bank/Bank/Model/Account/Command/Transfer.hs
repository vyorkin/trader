{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Command.Transfer
  ( Transfer(..)
  , transferId
  , amount
  , targetAccount
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data Transfer = Transfer
  { _transferId :: !UUID
  , _amount :: !Double
  , _targetAccount :: !UUID
  } deriving (Eq, Show)

makeLenses ''Transfer
deriveJSON defaultOptions ''Transfer

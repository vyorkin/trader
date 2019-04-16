{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.TransferCompleted
  ( TransferCompleted(..)
  , transferId
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data TransferCompleted = TransferCompleted
  { _transferId :: !UUID
  } deriving (Eq, Show)

makeLenses ''TransferCompleted
deriveJSON defaultOptions ''TransferCompleted

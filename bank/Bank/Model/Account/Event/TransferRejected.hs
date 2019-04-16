{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.TransferRejected
  ( TransferRejected(..)
  , transferId
  , reason
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data TransferRejected = TransferRejected
  { _transferId :: !UUID
  , _reason :: !Text
  } deriving (Eq, Show)

makeLenses ''TransferRejected
deriveJSON defaultOptions ''TransferRejected

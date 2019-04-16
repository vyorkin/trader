{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Customer.Event.CreationRejected
  ( CreationRejected(..)
  , rejectionReason
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data CreationRejected = CreationRejected
  { _rejectionReason :: !Text
  } deriving (Eq, Show)

makeLenses ''CreationRejected
deriveJSON defaultOptions ''CreationRejected

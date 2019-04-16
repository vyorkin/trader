{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Customer.Event.Created
  ( Created(..)
  , customerName
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data Created = Created
  { _customerName :: !Text
  } deriving (Eq, Show)

makeLenses ''Created
deriveJSON defaultOptions ''Created

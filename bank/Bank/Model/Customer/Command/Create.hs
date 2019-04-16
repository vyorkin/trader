{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Customer.Command.Create
  ( Create(..)
  , customerName
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data Create = Create
  { _customerName :: !Text
  } deriving (Eq, Show)

makeLenses ''Create
deriveJSON defaultOptions ''Create

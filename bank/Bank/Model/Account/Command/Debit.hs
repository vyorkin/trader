{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Command.Debit
  ( Debit(..)
  , amount
  , reason
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data Debit = Debit
  { _amount :: !Double
  , _reason :: !Text
  } deriving (Eq, Show)

makeLenses ''Debit
deriveJSON defaultOptions ''Debit

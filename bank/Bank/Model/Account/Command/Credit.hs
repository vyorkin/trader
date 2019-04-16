{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Command.Credit
  ( Credit(..)
  , amount
  , reason
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data Credit = Credit
  { _amount :: !Double
  , _reason :: !Text
  } deriving (Eq, Show)

makeLenses ''Credit
deriveJSON defaultOptions ''Credit

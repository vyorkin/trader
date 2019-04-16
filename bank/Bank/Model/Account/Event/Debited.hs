{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.Debited
  ( Debited(..)
  , amount
  , reason
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data Debited = Debited
  { _amount :: !Double
  , _reason :: !Text
  } deriving (Eq, Show)

makeLenses ''Debited
deriveJSON defaultOptions ''Debited

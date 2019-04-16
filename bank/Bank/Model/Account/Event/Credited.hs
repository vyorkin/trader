{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.Credited
  ( Credited(..)
  , amount
  , reason
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data Credited = Credited
  { _amount :: !Double
  , _reason :: !Text
  } deriving (Eq, Show)

makeLenses ''Credited
deriveJSON defaultOptions ''Credited

{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.Opened
  ( Opened(..)
  , owner
  , initialFunding
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data Opened = Opened
  { _owner :: !UUID
  , _initialFunding :: !Double
  } deriving (Eq, Show)

makeLenses ''Opened
deriveJSON defaultOptions ''Opened

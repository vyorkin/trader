{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Command.Open
  ( Open(..)
  , owner
  , initialFunding
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data Open = Open
  { _owner :: !UUID
  , _initialFunding :: !Double
  } deriving (Eq, Show)

makeLenses ''Open
deriveJSON defaultOptions ''Open

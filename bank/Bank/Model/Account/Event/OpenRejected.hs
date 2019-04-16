{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event.OpenRejected
  ( OpenRejected(..)
  , reason
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)

data OpenRejected = OpenRejected
  { _reason :: !Text
  } deriving (Eq, Show)

makeLenses ''OpenRejected
deriveJSON defaultOptions ''OpenRejected

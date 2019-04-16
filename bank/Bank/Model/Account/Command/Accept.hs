{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Command.Accept
  ( Accept(..)
  , transferId
  , amount
  , sourceAccount
  ) where

import Control.Lens (makeLenses)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (UUID)

data Accept = Accept
  { _transferId :: !UUID
  , _amount :: !Double
  , _sourceAccount :: !UUID
  } deriving (Eq, Show)

makeLenses ''Accept
deriveJSON defaultOptions ''Accept

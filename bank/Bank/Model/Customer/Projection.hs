{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Bank.Model.Customer.Projection
  ( Customer(..)
  , CustomerEvent(..)
  , customerName
  , handleEvent
  , projection
  ) where

import Control.Lens (makeLenses, (?~))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Eventful (Projection (..))
import SumTypes.TH (SumTypeTagOptions (..), constructSumType,
                    defaultSumTypeOptions, sumTypeOptionsTagOptions)

import Bank.Model.Customer.Event (Created (..), events)

data Customer = Customer
  { _customerName :: !(Maybe Text)
  } deriving (Eq, Show)

makeLenses ''Customer
deriveJSON defaultOptions ''Customer

constructSumType "CustomerEvent"
  (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags })
  events

deriving instance Show CustomerEvent
deriving instance Eq CustomerEvent

handleEvent :: Customer -> CustomerEvent -> Customer
handleEvent customer = \case
  CreatedCustomerEvent (Created name) -> customer & customerName ?~ name
  CreationRejectedCustomerEvent _     -> customer

projection :: Projection Customer CustomerEvent
projection = Projection (Customer Nothing) handleEvent

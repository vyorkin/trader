{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Customer.Aggregate
  ( CustomerCommand(..)
  , CustomerAggregate
  , handleCommand
  , aggregate
  ) where

import Control.Lens ((^.))
import Eventful (Aggregate (..))
import SumTypes.TH (SumTypeTagOptions (..), constructSumType,
                    defaultSumTypeOptions, sumTypeOptionsTagOptions)

import Bank.Model.Customer.Command (commands)
import Bank.Model.Customer.Command (Create (..))
import Bank.Model.Customer.Event (Created (..), CreationRejected (..))
import Bank.Model.Customer.Projection (Customer (..), CustomerEvent (..), customerName, projection)

constructSumType "CustomerCommand"
  (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags })
  commands

type CustomerAggregate = Aggregate Customer CustomerEvent CustomerCommand

handleCommand :: Customer -> CustomerCommand -> [CustomerEvent]
handleCommand customer (CreateCustomerCommand (Create name)) =
  case customer ^. customerName of
    Nothing -> [CreatedCustomerEvent $ Created name]
    Just _  -> [CreationRejectedCustomerEvent $ CreationRejected "Customer already exist"]

aggregate :: CustomerAggregate
aggregate = Aggregate handleCommand projection

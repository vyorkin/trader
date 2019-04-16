{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Bank.Model
  ( BankEvent(..)
  , BankCommand(..)
  ) where

import SumTypes.TH (SumTypeTagOptions (..), constructSumType,
                    defaultSumTypeOptions, sumTypeOptionsTagOptions)

import qualified Bank.Model.Account.Event as Account (events)
import qualified Bank.Model.Customer.Event as Customer (events)

import qualified Bank.Model.Account.Command as Account (commands)
import qualified Bank.Model.Customer.Command as Customer (commands)

constructSumType "BankEvent"
  (defaultSumTypeOptions { sumTypeOptionsTagOptions = ConstructTagName (++ "Event") })
  (Account.events ++ Customer.events)

deriving instance Eq BankEvent
deriving instance Show BankEvent

constructSumType "BankCommand"
  (defaultSumTypeOptions { sumTypeOptionsTagOptions = ConstructTagName (++ "Command") })
  (Account.commands ++ Customer.commands)

deriving instance Eq BankCommand
deriving instance Show BankCommand

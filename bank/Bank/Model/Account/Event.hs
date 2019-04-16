{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Event
  ( events
  , module Bank.Model.Account.Event.Credited
  , module Bank.Model.Account.Event.CreditedFromTransfer
  , module Bank.Model.Account.Event.DebitRejected
  , module Bank.Model.Account.Event.Debited
  , module Bank.Model.Account.Event.OpenRejected
  , module Bank.Model.Account.Event.Opened
  , module Bank.Model.Account.Event.TransferCompleted
  , module Bank.Model.Account.Event.TransferRejected
  , module Bank.Model.Account.Event.TransferStarted
  ) where

import Language.Haskell.TH (Name)

import Bank.Model.Account.Event.Credited (Credited)
import Bank.Model.Account.Event.CreditedFromTransfer (CreditedFromTransfer)
import Bank.Model.Account.Event.Debited (Debited)
import Bank.Model.Account.Event.DebitRejected (DebitRejected)
import Bank.Model.Account.Event.Opened (Opened)
import Bank.Model.Account.Event.OpenRejected (OpenRejected)
import Bank.Model.Account.Event.TransferCompleted (TransferCompleted)
import Bank.Model.Account.Event.TransferRejected (TransferRejected)
import Bank.Model.Account.Event.TransferStarted (TransferStarted)

events :: [Name]
events =
  [ ''Credited
  , ''CreditedFromTransfer
  , ''DebitRejected
  , ''Debited
  , ''OpenRejected
  , ''Opened
  , ''TransferCompleted
  , ''TransferRejected
  , ''TransferStarted
  ]

{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Account.Command
  ( commands
  , module Bank.Model.Account.Command.Open
  , module Bank.Model.Account.Command.Credit
  , module Bank.Model.Account.Command.Debit
  , module Bank.Model.Account.Command.Transfer
  , module Bank.Model.Account.Command.Accept
  ) where

import Language.Haskell.TH (Name)

import Bank.Model.Account.Command.Open (Open)
import Bank.Model.Account.Command.Credit (Credit)
import Bank.Model.Account.Command.Debit (Debit)
import Bank.Model.Account.Command.Transfer (Transfer)
import Bank.Model.Account.Command.Accept (Accept)

commands :: [Name]
commands =
  [ ''Open
  , ''Credit
  , ''Debit
  , ''Transfer
  , ''Accept
  ]

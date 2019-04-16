{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Bank.Model.Account.Aggregate
  ( AccountCommand(..)
  , AccountAggregate
  , handleCommand
  , aggregate
  ) where

import Control.Lens ((^.))
import Eventful (Aggregate (..))
import SumTypes.TH (SumTypeTagOptions (..), constructSumType,
                    defaultSumTypeOptions, sumTypeOptionsTagOptions)

import Bank.Model.Account.Command (commands)
import Bank.Model.Account.Command.Accept (Accept (..))
import Bank.Model.Account.Command.Credit (Credit (..))
import Bank.Model.Account.Command.Debit (Debit (..))
import Bank.Model.Account.Command.Open (Open (..))
import Bank.Model.Account.Command.Transfer (Transfer (..))

import Bank.Model.Account.Event.Credited (Credited (..))
import Bank.Model.Account.Event.CreditedFromTransfer (CreditedFromTransfer (..))
import Bank.Model.Account.Event.Debited (Debited (..))
import Bank.Model.Account.Event.DebitRejected (DebitRejected (..))
import Bank.Model.Account.Event.Opened (Opened (..))
import Bank.Model.Account.Event.OpenRejected (OpenRejected (..))
import Bank.Model.Account.Event.TransferCompleted (TransferCompleted (..))
import Bank.Model.Account.Event.TransferRejected (TransferRejected (..))
import Bank.Model.Account.Event.TransferStarted (TransferStarted (..))

import Bank.Model.Account.Projection (Account (..), AccountEvent (..), owner,
                                      projection, availableBalance)

constructSumType "AccountCommand"
  (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags })
  commands

type AccountAggregate = Aggregate Account AccountEvent AccountCommand

handleCommand :: Account -> AccountCommand -> [AccountEvent]
handleCommand acc = \case
  OpenAccountCommand Open{..} ->
    case acc ^. owner of
      Just _  -> [OpenRejectedAccountEvent $ OpenRejected "Account already open"]
      Nothing ->
        if _initialFunding < 0
        then [OpenRejectedAccountEvent $ OpenRejected $ "Invalid initial deposit: " <> show _initialFunding]
        else [OpenedAccountEvent Opened{..}]
  CreditAccountCommand Credit{..} ->
    [CreditedAccountEvent Credited{..}]
  DebitAccountCommand Debit{..} ->
    if availableBalance acc - _amount < 0
    then [DebitRejectedAccountEvent $ DebitRejected $ availableBalance acc]
    else [DebitedAccountEvent $ Debited{..}]
  _ ->
    []

aggregate :: AccountAggregate
aggregate = Aggregate handleCommand projection

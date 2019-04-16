{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

module Bank.Model.Account.Projection
  ( -- * Data types
    Account(..)
  , AccountEvent(..)
    -- * Operations
  , emptyAccount
  , availableBalance
  , findTransfer
    -- * Lenses
  , owner
  , balance
  , pendingTransfers
    -- * Projection
  , handleEvent
  , projection
  ) where

import Control.Lens (cons, makeLenses, view, (%~), (+~), (-~), (.~), (?~), (^.))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.List (delete)
import Eventful (Projection (..), UUID)
import SumTypes.TH (SumTypeTagOptions (..), constructSumType,
                    defaultSumTypeOptions, sumTypeOptionsTagOptions)

import Bank.Model.Account.Data.PendingTransfer (PendingTransfer (..), amount,
                                                transferId)

import Bank.Model.Account.Event (events)
import Bank.Model.Account.Event.Credited (Credited (..))
import Bank.Model.Account.Event.CreditedFromTransfer (CreditedFromTransfer (..))
import Bank.Model.Account.Event.Debited (Debited (..))
import Bank.Model.Account.Event.DebitRejected (DebitRejected (..))
import Bank.Model.Account.Event.Opened (Opened (..))
import Bank.Model.Account.Event.OpenRejected (OpenRejected (..))
import Bank.Model.Account.Event.TransferCompleted (TransferCompleted (..))
import Bank.Model.Account.Event.TransferRejected (TransferRejected (..))
import Bank.Model.Account.Event.TransferStarted (TransferStarted (..))

data Account = Account
  { _owner :: !(Maybe UUID)
  , _balance :: !Double
  , _pendingTransfers :: ![PendingTransfer]
  } deriving (Eq, Show)

makeLenses ''Account
deriveJSON defaultOptions ''Account

emptyAccount :: Account
emptyAccount = Account Nothing 0 []

-- | Account balance minus pending balance (funds locked due to pending transfers).
availableBalance :: Account -> Double
availableBalance acc = acc ^. balance - pendingBalance (acc ^. pendingTransfers)

pendingBalance :: [PendingTransfer] -> Double
pendingBalance []        = 0
pendingBalance transfers = sum (view amount <$> transfers)

findTransfer :: [PendingTransfer] -> UUID -> Maybe PendingTransfer
findTransfer transfers tid = find ((== tid) . view transferId) transfers

constructSumType "AccountEvent"
  (defaultSumTypeOptions { sumTypeOptionsTagOptions = AppendTypeNameToTags })
  events

deriving instance Show AccountEvent
deriving instance Eq AccountEvent

handleEvent :: Account -> AccountEvent -> Account
handleEvent acc = \case
  OpenedAccountEvent Opened{..} ->
    acc
    & owner ?~ _owner
    & balance .~ _initialFunding
  OpenRejectedAccountEvent _ ->
    acc
  CreditedAccountEvent Credited{..} ->
    acc & balance +~ _amount
  DebitedAccountEvent Debited{..} ->
    acc & balance -~ _amount
  DebitRejectedAccountEvent _ ->
    acc
  TransferStartedAccountEvent TransferStarted{..} ->
    acc & pendingTransfers %~ cons PendingTransfer{..}
  TransferCompletedAccountEvent TransferCompleted{..} ->
    let
      update t =
        acc
        & balance -~ t ^. amount
        & pendingTransfers %~ delete t
      transfers = acc ^. pendingTransfers
      transfer = findTransfer transfers _transferId
    in
      maybe acc update transfer
  TransferRejectedAccountEvent TransferRejected{..} ->
    let
      oldTransfers = acc ^. pendingTransfers
      transfer = findTransfer oldTransfers _transferId
      newTransfers = maybe oldTransfers (flip delete oldTransfers) transfer
    in
      acc & pendingTransfers .~ newTransfers
  CreditedFromTransferAccountEvent CreditedFromTransfer{..} ->
    acc & balance +~ _amount

projection :: Projection Account AccountEvent
projection = Projection emptyAccount handleEvent

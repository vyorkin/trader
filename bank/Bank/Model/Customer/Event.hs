{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Customer.Event
  ( events
  , module Bank.Model.Customer.Event.Created
  , module Bank.Model.Customer.Event.CreationRejected
  ) where

import Language.Haskell.TH (Name)

import Bank.Model.Customer.Event.Created (Created (..))
import Bank.Model.Customer.Event.CreationRejected (CreationRejected (..))

events :: [Name]
events =
  [ ''Created
  , ''CreationRejected
  ]

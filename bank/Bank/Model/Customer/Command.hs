{-# LANGUAGE TemplateHaskell #-}

module Bank.Model.Customer.Command
  ( commands
  , module Bank.Model.Customer.Command.Create
  ) where

import Language.Haskell.TH (Name)

import Bank.Model.Customer.Command.Create (Create (..))

commands :: [Name]
commands =
  [ ''Create
  ]

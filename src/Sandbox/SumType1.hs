{-# LANGUAGE TemplateHaskell #-}

module Sandbox.SumType1 where

import SumTypes.TH

data TypeA = TypeA deriving (Show, Eq)
data TypeB = TypeB deriving (Show, Eq)
data TypeC = TypeC deriving (Show, Eq)

constructSumType "MySum"
  defaultSumTypeOptions
  [ ''TypeA
  , ''TypeB
  , ''TypeC
  ]
constructSumType "OtherSum"
  defaultSumTypeOptions
  [ ''TypeA
  , ''TypeB
  ]
sumTypeConverter "otherToMySum" ''OtherSum ''MySum
partialSumTypeConverter "mySumTypeToOtherSum" ''MySum ''OtherSum

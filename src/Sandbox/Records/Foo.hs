{-# LANGUAGE TemplateHaskell #-}

module Sandbox.Records.Foo
  ( Foo(..)
  , foo
  , bar
  ) where

import Control.Lens (makeLenses)

data Foo = Foo
  { _foo :: !Text
  , _bar :: !Int
  } deriving (Eq, Show)

makeLenses ''Foo

{-# LANGUAGE TemplateHaskell #-}

module Sandbox.Records.Bar
  ( Bar(..)
  , foo
  , bar
  ) where

import Control.Lens (makeLenses)

data Bar = Bar
  { _foo :: !Int
  , _bar :: !Text
  } deriving (Eq, Show)

makeLenses ''Bar

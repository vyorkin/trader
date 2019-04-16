{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Sandbox.Records1 where

import Control.Lens (makeLenses, view, (.~), (?~), (^.))
import qualified Data.Text as Text

import Sandbox.Records.Bar (Bar (..))
import Sandbox.Records.Foo (Foo (..))

data Baz = Baz
  { _foo :: !Text
  , _bar :: !Int
  } deriving (Eq, Show)

makeLenses ''Baz

blah :: Baz -> Foo -> Baz
blah baz Foo{..} =
  baz
  & foo .~ _foo <> "!"
  & bar .~ _bar + 1

fooToBar :: Foo -> Bar
fooToBar Foo{..} = Bar _bar _foo

barToFoo :: Bar -> Foo
barToFoo Bar{..} = Foo _bar _foo

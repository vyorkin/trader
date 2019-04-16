{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Sandbox.Aeson1 where

import Data.Aeson

data Foo = Foo
 { bar :: !(Maybe Bar)
 } deriving (Generic, Show)

instance FromJSON Foo where
  parseJSON = withObject "Foo" $ \o -> do
    bar <- optional (o .: "bar")
    return Foo{..}

instance ToJSON Foo

data Bar = Bar | Blah
  deriving (Generic, Show)

instance FromJSON Bar
instance ToJSON Bar

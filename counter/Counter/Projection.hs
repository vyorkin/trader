{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Counter.Projection
  ( Counter(..)
  , value
  , handleEvent
  , projection
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Control.Lens (makeLenses, (+~), (-~))
import Eventful (Projection (..))

import Counter.Event (CounterEvent (..))

data Counter = Counter
  { _value :: Int
  } deriving (Show, Eq)

makeLenses ''Counter
deriveJSON defaultOptions ''Counter

handleEvent :: Counter -> CounterEvent -> Counter
handleEvent counter = \case
  Incremented n -> counter & value +~ n
  Decremented n -> counter & value -~ n

projection :: Projection Counter CounterEvent
projection = Projection
  { projectionSeed = Counter 0
  , projectionEventHandler = handleEvent
  }

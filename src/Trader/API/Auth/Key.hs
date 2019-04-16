{-# LANGUAGE TemplateHaskell #-}

module Trader.API.Auth.Key
  ( Key(..)
  , load
  , public
  , secret
  , toByteString
  ) where

import Text.Show (show, Show)
import qualified Data.Text as Text (unpack, pack)
import Control.Lens (makeLenses)
import System.Environment (lookupEnv)

-- | Represents an API key.
data Key = Key
  { _public :: !Text -- ^ Public key.
  , _secret :: !Text -- ^ Secret key.
  }

makeLenses ''Key

-- Don't derive a `Show` instance.
-- We don't ever want to reveal the secret part of the API key.
-- Instead, provide a manual instance.

instance Show Key where
  show (Key pub _) = "Key {- " ++ Text.unpack pub ++ " -}"

-- | Load API key from the system environment.
load :: IO (Maybe Key)
load = runMaybeT $ do
  pub <- Text.pack <$> MaybeT (lookupEnv "API_KEY")
  sec <- Text.pack <$> MaybeT (lookupEnv "API_SECRET")
  return $ Key pub sec

-- | Converts `Key` to `ByteString`.
toByteString :: Key -> ByteString
toByteString (Key pub _) = encodeUtf8 pub

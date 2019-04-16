module Trader.Data.Account
  ( Account(..)
  , AccountId
  ) where

import Data.Aeson (FromJSON, ToJSON)

-- | Account.
data Account = Account
  { accountId :: !Account
  }

-- | Account id.
type AccountId = Int

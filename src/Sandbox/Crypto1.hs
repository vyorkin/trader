module Sandbox.Crypto1 where

import Data.ByteArray
import Crypto.Hash
import Crypto.Hash.Algorithms (SHA1, SHA256)
import Crypto.MAC.HMAC (hmac, hmacGetDigest)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)

sign
  :: (ByteArrayAccess secret, ByteArrayAccess message)
  => secret
  -> message
  -> Digest SHA256
sign s = hmacGetDigest . hmac s

secret :: ByteString
secret = "h4vWHdGiJoqc0jePnIjdSF21tg0icz7RjNr80-rSOnpKLeto"

hashes :: ByteString -> IO ()
hashes s = do
  putStrLn $ "  sha1(" ++ show s ++ ") = " ++ show (hashWith SHA1 s)
  putStrLn $ "sha256(" ++ show s ++ ") = " ++ show (hashWith SHA256 s)
  putStrLn $ "hmac-sha256(" ++ Char8.unpack secret ++ ", " ++ show s ++ ") = " ++ show (sign secret s)

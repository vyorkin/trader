module Sandbox.Req1 where

import Data.Aeson
import Network.HTTP.Req

main :: IO ()
main = runReq defaultHttpConfig $ do
  let payload = object
        [ "foo" .= (10 :: Int)
        , "bar" .= (20 :: Int)
        ]
  r <- req POST
    (https "httpbin.org" /: "post")
    (ReqBodyJson payload)
    jsonResponse
    mempty
  liftIO $ print (responseBody r :: Value)

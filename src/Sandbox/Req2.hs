{-# LANGUAGE DeriveGeneric #-}

module Sandbox.Req2 where

import Data.Aeson
import Network.HTTP.Req

ex1 :: IO ()
ex1 = runReq defaultHttpConfig $ do
  let bytes :: Int
      bytes = 3
  res <- req GET
    (https "httpbin.org" /: "bytes" /~ bytes)
    NoReqBody
    bsResponse
    mempty
  liftIO $ print (responseBody res)

ex2 :: IO ()
ex2 = runReq defaultHttpConfig $ do
  res <- req GET
    (https "httpbin.org" /: "bytes" /~ (3 :: Int))
    NoReqBody
    bsResponse
    ("param" =: (10 :: Int))
  liftIO $ print (responseBody res)

data Foo = Foo
  { foo :: Int
  , bar :: Text
  } deriving (Show, Generic)

instance ToJSON Foo
instance FromJSON Foo

ex3 :: IO ()
ex3 = runReq defaultHttpConfig $ do
  let myFoo = Foo { foo = 8, bar = "bar" }
  res <- req POST
    (https "httpbin.org" /: "post")
    (ReqBodyJson myFoo)
    jsonResponse
    mempty
  liftIO $ print (responseBody res :: Value)

ex4 :: IO ()
ex4 = runReq defaultHttpConfig $ do
  res <- req GET
    (https "httpbin.org" /: "headers")
    NoReqBody
    bsResponse
    (header "foo" "bar")
  liftIO $ print (responseBody res)

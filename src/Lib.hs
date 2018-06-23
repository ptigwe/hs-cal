{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( msReceiveAuthAndDie
  , login
  ) where

import Key
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Web.Browser
import Network.OAuth.OAuth2
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.ByteString.Builder as BB
import qualified URI.ByteString as URI
import qualified Data.Map.Strict as M

msReceiveAuthAndDie :: IO (Maybe B.ByteString)
msReceiveAuthAndDie = do
  toDie <- newEmptyMVar
  result <- newEmptyMVar
  race_ (takeMVar toDie) $ run 5000 $ \req send ->
    if pathInfo req == ["login", "authorized"]
    then do
        putMVar toDie ()
        putMVar result $ queryString req
        send $ responseLBS status200 [(hContentType, "text/html")] "<body onload=\"window.close()\"></body>"
    else do
      send $ responseLBS status200 [] "Still alive!"
  res <- takeMVar result
  return (fromMaybe Nothing . lookup "code" $ res)

login :: OAuth2 -> IO (B.ByteString)
login key = do
  let uri = URI.serializeURIRef . buildAuthURL . authorizationUrl $ key
  openBrowser . unpack . BB.toLazyByteString $ uri
  res <- msReceiveAuthAndDie
  case res of
    Nothing -> login key
    Just r -> return r

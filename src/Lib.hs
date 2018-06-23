{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( msReceiveAuthAndDie
  , login
  , fetchToken
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
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified URI.ByteString as URI
import qualified Data.Map.Strict as M
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS

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

login :: OAuth2 -> IO (T.Text)
login key = do
  let uri = URI.serializeURIRef . buildAuthURL . authorizationUrl $ key
  openBrowser . unpack . BB.toLazyByteString $ uri
  res <- msReceiveAuthAndDie
  case res of
    Nothing -> login key
    Just r -> return $ T.pack . C.unpack $ r

fetchToken :: OAuth2 -> T.Text -> IO ()
fetchToken key authKey = do
  mgr <- newManager tlsManagerSettings
  token <- fetchAccessToken mgr key (ExchangeToken authKey)
  print token

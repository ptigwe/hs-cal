{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( msReceiveAuthAndDie
  , login
  , fetchToken
  , fetchUserInfo
  ) where

import Key
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Text as T
import qualified URI.ByteString as URI
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import qualified Network.OAuth.OAuth2 as OAuth
import Network.OAuth.OAuth2.TokenRequest (Errors)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wreq
import Web.Browser

msReceiveAuthAndDie :: IO (Maybe B.ByteString)
msReceiveAuthAndDie = do
  toDie <- newEmptyMVar
  result <- newEmptyMVar
  race_ (takeMVar toDie) $ run 5000 $ \req send ->
    if pathInfo req == ["login", "authorized"]
    then do
        putMVar toDie ()
        putMVar result $ queryString req
        send $ responseLBS status200 [(hContentType, "text/html")] "<body onload=\"close()\"></body>"
    else do
      send $ responseLBS status200 [] "Still alive!"
  res <- takeMVar result
  return (fromMaybe Nothing . lookup "code" $ res)

login :: OAuth.OAuth2 -> IO (T.Text)
login key = do
  let uri = URI.serializeURIRef . buildAuthURL . OAuth.authorizationUrl $ key
  openBrowser . unpack . toLazyByteString $ uri
  res <- msReceiveAuthAndDie
  case res of
    Nothing -> login key
    Just r -> return $ T.pack . C.unpack $ r

fetchToken :: OAuth.OAuth2 -> T.Text -> IO (OAuth.OAuth2Result Errors OAuth.OAuth2Token)
fetchToken key authKey = do
  mgr <- newManager tlsManagerSettings
  OAuth.fetchAccessToken mgr key (OAuth.ExchangeToken authKey)

fetchUserInfo :: OAuth.OAuth2Token -> IO ()
fetchUserInfo OAuth.OAuth2Token{..} = do
  let opts = defaults & header "Accept" .~ ["application/json"]
                      & header "Authorization" .~ [C.append "Bearer " . C.pack . T.unpack . OAuth.atoken $ accessToken]
  getWith opts "https://graph.microsoft.com/v1.0/me" >>= print


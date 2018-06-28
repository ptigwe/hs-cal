{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Config
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe
import qualified Network.OAuth.OAuth2 as OAuth

tryLogin :: IO (B.ByteString)
tryLogin = do
  conf <- fmap fromJust readConfig
  let key = microKey conf

  login key >>= fetchToken key >>= (\val -> do
    case val of
      Left _ -> return ""
      Right x -> return . T.encodeUtf8 . OAuth.atoken . OAuth.accessToken $ x)

getToken :: IO (B.ByteString)
getToken = do
  token <- tmpToken
  if B.null token
     then tryLogin
     else return token

main :: IO ()
main = do
  getToken >>= fetchUserInfo

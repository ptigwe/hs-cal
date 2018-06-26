{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Config
import Data.Maybe

main :: IO ()
main = do
  conf <- fmap fromJust readConfig
  let key = microKey conf

  login key >>= fetchToken key >>= (\ val -> do
    case val of
      Left _ -> print "Err"
      Right x -> fetchUserInfo x)

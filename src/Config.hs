{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Config
  ( Config
  , microKey
  , readConfig
  , addScope
  , tmpToken
  )where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Char8 as B
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.OAuth.OAuth2 as OAuth
import qualified URI.ByteString as URI

data Config = Config { clientID :: T.Text
                     , clientSecret :: T.Text
                     , redirectURI :: T.Text
                     , authorityURL :: T.Text
                     , authEndpoint :: T.Text
                     , tokenEndpoint :: T.Text
                     , resource :: T.Text
                     , apiVersion :: T.Text}
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

parseStrictURI :: T.Text -> Maybe (URI.URIRef URI.Absolute)
parseStrictURI = rightToMaybe . URI.parseURI URI.strictURIParserOptions . T.encodeUtf8

microKey :: Config -> OAuth.OAuth2
microKey Config{..} = OAuth.OAuth2 {..}
  where
    oauthClientId = clientID
    oauthClientSecret = clientSecret
    oauthCallback = parseStrictURI redirectURI
    oauthOAuthorizeEndpoint = fromJust . parseStrictURI $ authorityURL `mappend` authEndpoint
    oauthAccessTokenEndpoint = fromJust . parseStrictURI $ authorityURL `mappend` tokenEndpoint

addScope :: URI.URI -> URI.URI
addScope = OAuth.appendQueryParams [("scope", scope), ("state", "abcde")]
  where
    scope = "User.Read Calendars.Read"

configFile :: FilePath
configFile = "config.json"

readConfig :: IO (Maybe Config)
readConfig = do
  cont <- B.readFile configFile
  return (decodeStrict cont)

tmpToken :: IO (B.ByteString)
tmpToken = B.readFile "token"

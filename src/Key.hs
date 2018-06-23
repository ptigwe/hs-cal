{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Key where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Aeson
import Data.Either.Combinators (rightToMaybe)
import Network.OAuth.OAuth2
import qualified URI.ByteString as URI
import qualified URI.ByteString.QQ as QQ

data Config = Config { clientID :: T.Text
                     , clientSecret :: T.Text
                     , redirectURI :: T.Text
                     , authorityURL :: T.Text
                     , authEndpoint :: T.Text
                     , tokenEndpoint :: T.Text
                     , resource :: T.Text
                     , apiVersion :: T.Text
                     , scope :: T.Text }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o ->
    Config <$> o .: "client_id"
           <*> o .: "client_secret"
           <*> o .: "redirect_uri"
           <*> o .: "authority_url"
           <*> o .: "auth_endpoint"
           <*> o .: "token_endpoint"
           <*> o .: "resource"
           <*> o .: "api_version"
           <*> o .: "scope"

parseStrictURI :: T.Text -> Maybe (URI.URIRef URI.Absolute)
parseStrictURI = rightToMaybe . URI.parseURI URI.strictURIParserOptions . C.pack . T.unpack

microKey :: Config -> OAuth2
microKey Config{..} = OAuth2 {..}
  where
    oauthClientId = clientID
    oauthClientSecret = clientSecret
    oauthCallback = parseStrictURI redirectURI
    oauthOAuthorizeEndpoint = fromJust . parseStrictURI $ authorityURL `mappend` authEndpoint
    oauthAccessTokenEndpoint = fromJust . parseStrictURI $ authorityURL `mappend` tokenEndpoint

buildAuthURL :: URI.URI -> URI.URI
buildAuthURL x@URI.URI{..} = x{URI.uriQuery=newPairs}
  where
    newPairs = uriQuery `mappend` (URI.Query [("scope", "User.Read"), ("state", "abcde")])

configFile :: FilePath
configFile = "config.json"

readConfig :: IO (Maybe Config)
readConfig = do
  cont <- B.readFile configFile
  return (decodeStrict cont)

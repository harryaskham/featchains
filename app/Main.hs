{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding  as LE
import qualified Data.Text.Encoding  as E
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Network.Wreq
    ( getWith,
      postWith,
      defaults,
      header,
      param,
      responseBody,
      FormParam((:=)) )
import Data.Aeson ( FromJSON(parseJSON), decode, (.:), withObject )
import Control.Lens ( (&), (^.), (.~) )
import Data.Text.Encoding.Base64 ( encodeBase64 )

{- TODO
Fetch song info
Figure out collaborators representation
Artist is name + ID
Write BFS graph scraper
Define data model
Scrape + persist graph
Look at graphDB options
Build graph inmem
On the fly bfs / dfs
All-pairs caching?
-}

clientId :: T.Text
clientId = "28342f3712b74711af0f925204a8aae1"

clientSecret :: IO T.Text
clientSecret = do
  content <- readFile "secret"
  return $ T.pack $ head (lines content)

apiUri :: String
apiUri = "https://api.spotify.com/v1"

-- Wrapper type for the API token we'll use
newtype Token = Token T.Text deriving (Eq, Show)

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v -> Token <$> v .: "access_token"

-- Get a client OAuth token
-- Needs to concat ID and secret, base64 encoded
-- Pulls the token out of the JSON response
getToken :: T.Text -> IO Token
getToken secret = do
  let tokenUri = "https://accounts.spotify.com/api/token"
  let opts = defaults & header "Authorization"
                      .~ [E.encodeUtf8 $ "Basic " <> (encodeBase64 (clientId <> ":" <> secret))]
  r <-
    postWith opts tokenUri
    $ [("grant_type" :: B.ByteString) := ("client_credentials" :: B.ByteString)]
  case decode $ r ^. responseBody of
    Just token -> return token
    Nothing -> error "Invalid token response"

searchUri :: String
searchUri = apiUri <> "/search"

search :: IO LB.ByteString
search = do
  let opts = defaults & param "q" .~ ["Jam Baxter"]
                      & param "type" .~ ["artist"]
  r <- getWith opts searchUri
  return $ r ^. responseBody

getArtistsUri :: String -> String
getArtistsUri artistId = apiUri <> "/artists/" <> artistId

getArtist :: Token  -> String -> IO LB.ByteString
getArtist (Token token) artistId  = do
  let opts = defaults & header "Authorization" .~ [E.encodeUtf8 $ "Bearer " <> token]
  r <- getWith opts $ getArtistsUri artistId
  return $ r ^. responseBody

main :: IO ()
main = do
  secret <- clientSecret
  token <- getToken secret
  print token
  artist <- getArtist token "0TnOYISbd1XYRBk9myaseg"
  print artist

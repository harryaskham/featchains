{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.List ( foldl' )
import Data.Maybe ( catMaybes )
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding  as LE
import qualified Data.Text.Encoding  as E
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.Wreq
    ( getWith,
      postWith,
      defaults,
      header,
      param,
      responseBody,
      responseStatus,
      statusCode,
      Response,
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
artistname sometimes has semicolons; ampersands, etc

SCRAPER
=======
pick a seed artist
get all songs
filter to those with feats or collabs
add edges between artists
mark original as done
add collabs to queue for the same
parallelisation might be wasteful when building the graph
so we need artist-level locks, only one thread can be doing one artist at once
simple artist-request caching should be sufficient as long as all threads can access it
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

search :: Token -> T.Text -> T.Text -> Int -> Int -> IO (Response LB.ByteString)
search (Token token) query queryType limit offset = do
  let uri = apiUri <> "/search"
      opts = defaults & header "Authorization" .~ [E.encodeUtf8 $ "Bearer " <> token]
                      & param "q" .~ [query]
                      & param "type" .~ [queryType]
                      & param "limit" .~ [T.pack $ show limit]
                      & param "offset" .~ [T.pack $ show offset]
  getWith opts uri

newtype ArtistId = ArtistId T.Text deriving (Show, Eq, Ord)
newtype ArtistName = ArtistName T.Text deriving (Show, Eq, Ord)
data Artist = Artist ArtistId ArtistName deriving (Show, Eq, Ord)
newtype TrackId = TrackId T.Text deriving (Show, Eq, Ord)
newtype TrackName = TrackName T.Text deriving (Show, Eq, Ord)
data Track = Track TrackId TrackName [Artist] deriving (Show, Eq, Ord)
newtype TrackList = TrackList [Track] deriving (Show, Eq, Ord, Semigroup, Monoid)

instance FromJSON TrackList where
  parseJSON =
    withObject "TrackList"
    $ \v -> TrackList <$> (v .: "tracks" >>= (.: "items"))

instance FromJSON Track where
  parseJSON =
    withObject "Track"
    $ \v -> Track
            <$> (TrackId <$> v .: "id")
            <*> (TrackName <$> v .: "name")
            <*> (v .: "artists")

instance FromJSON Artist where
  parseJSON =
    withObject "Artist"
    $ \v -> Artist
            <$> (ArtistId <$> v .: "id")
            <*> (ArtistName <$> v .: "name")

-- Get tracks for a given artist
-- Right now returns up to 50 tracks
-- Remixes will mess up the stats
getArtistTracks :: Token -> Artist -> IO TrackList
getArtistTracks token (Artist _ (ArtistName name)) = do
  let offsets = [0]
  r <- traverse (search token ("artist:" <> name) "track" 50) offsets
  let parseR r = case r ^. responseStatus . statusCode of
                   200 -> case decode $ r ^. responseBody of
                                Just tl -> Just tl
                                Nothing -> error "Invalid body"
                   404 -> Nothing
                   _ -> error "Unexpected status code"
      tls = catMaybes $ parseR <$> r
  return $ foldl' (<>) (TrackList []) tls

-- Extract the unique artists from a tracklist
uniqueArtists :: TrackList -> S.Set Artist
uniqueArtists (TrackList tl) = S.fromList $ concat $ artists <$> tl
  where
    artists (Track _ _ as) = as

-- Get the artist's collaborators without the artist present
getCollaborators :: Token -> Artist -> IO (S.Set Artist)
getCollaborators token artist = do
  tracks <- getArtistTracks token artist
  let artists = uniqueArtists tracks
  return $ S.delete artist artists

main :: IO ()
main = do
  secret <- clientSecret
  token <- getToken secret
  print token
  -- Need a seed artist
  let jam = Artist (ArtistId "6ST2sHlQoWYjxkIVnuW2mr") (ArtistName "Jam Baxter")
  collaborators <- getCollaborators token jam
  print collaborators

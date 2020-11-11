{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.List ( foldl', nub )
import Data.Maybe ( catMaybes )
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Concurrent.MVar
    ( newMVar, putMVar, readMVar, takeMVar, MVar )
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding  as LE
import qualified Data.Text.Encoding  as E
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBC
import Control.Monad ( replicateM_ )
import Control.Monad.State
    ( MonadIO(liftIO),
      MonadState(get),
      StateT(runStateT),
      gets,
      evalStateT )
import Network.Wreq
    (responseHeader,  getWith,
      postWith,
      defaults,
      header,
      param,
      responseBody,
      responseStatus,
      statusCode,
      checkResponse,
      Response,
      FormParam((:=)) )
import Data.Aeson ( FromJSON(parseJSON), decode, (.:), withObject )
import Control.Lens
    ( (&), (^.), view, (.~), over, set, makeLenses )
import Data.Text.Encoding.Base64 ( encodeBase64 )
import Control.Concurrent ( threadDelay, forkIO )

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
      opts = defaults & header "Authorization"
                      .~ [E.encodeUtf8 $ "Basic " <> (encodeBase64 (clientId <> ":" <> secret))]
  r <-
    postWith opts tokenUri
    $ [("grant_type" :: B.ByteString) := ("client_credentials" :: B.ByteString)]
  case decode $ r ^. responseBody of
    Just token -> return token
    Nothing -> error "Invalid token response"

-- Issue a GET search request
search :: Token -> T.Text -> T.Text -> Int -> Int -> IO (Response LB.ByteString)
search (Token token) query queryType limit offset = do
  let uri = apiUri <> "/search"
      opts = defaults & header "Authorization" .~ [E.encodeUtf8 $ "Bearer " <> token]
                      & param "q" .~ [query]
                      & param "type" .~ [queryType]
                      & param "limit" .~ [T.pack $ show limit]
                      & param "offset" .~ [T.pack $ show offset]
                      & checkResponse .~ (Just $ \_ _ -> return ())
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
getArtistTracks token artist@(Artist _ (ArtistName name)) = do
  r <- search token ("artist:" <> name) "track" 50 0
  case r ^. responseStatus . statusCode of
    -- Success - parse the body
    200 -> case decode $ r ^. responseBody of
             Just tl -> return tl
             Nothing -> error "Invalid body"
    -- No results for query / no more songs
    404 -> return (TrackList [])
    -- Need to rate-limit, retry after a second
    429 -> do
      let waitFor = r ^. responseHeader "Retry-After"
          waitForSecs = read $ T.unpack $ E.decodeUtf8 waitFor
      threadDelay $ 1000000 * waitForSecs
      getArtistTracks token artist
    _ -> error "Unexpected status code"

-- Extract the unique artists from a tracklist
uniqueArtists :: TrackList -> S.Set Artist
uniqueArtists (TrackList tl) = S.fromList $ concat $ artists <$> tl
  where
    artists (Track _ _ as) = as

-- Represent a collaboration between artists on a given track
data Collaboration = Collaboration Artist Artist Track deriving (Show, Eq, Ord)

-- Gets all collaborations for an artist
-- Gets unidirectional collaborations
-- For songs with many people, does all-pairs
-- Also does not assume the passed-in artist is the first artist
getCollaborations :: Token -> Artist -> IO [Collaboration]
getCollaborations token artist = do
  TrackList tracks <- getArtistTracks token artist
  let getCollabs t@(Track _ _ as) = nub [Collaboration x y t | x <- as, y <- as]
      collabs = concatMap getCollabs tracks
  return $ filter (\(Collaboration a1 a2 _) -> a1 /= a2) collabs

getCollaborators :: Collaboration -> [Artist]
getCollaborators (Collaboration a1 a2 _) = [a1, a2]

data Scraper = Scraper { _queue :: MVar (SQ.Seq Artist)
                       , _collaborations :: MVar (SQ.Seq Collaboration)
                       , _done :: MVar (S.Set Artist)
                       , _seen :: MVar (S.Set Artist)
                       , _token :: Token
                       }
makeLenses ''Scraper

stepScraper :: StateT Scraper IO ()
stepScraper = do
  -- Pull out useful state
  s <- get
  qM <- gets (view queue)
  doneM <- gets (view done)
  seenM <- gets (view seen)
  collaborationsM <- gets (view collaborations)

  -- Block for access to the queue
  q <- liftIO $ takeMVar qM

  -- We got the queue handle, proceed
  case SQ.viewl q of
    a SQ.:< q -> do
      -- Return the queue handle before doing useful work with the artist
      -- This is what enables multiple workers to do IO at same time
      liftIO $ putMVar qM q

      -- Pull all collaborative tracks for this artist
      cs <- liftIO $ getCollaborations (s ^. token) a

      -- Track that we scraped this artist, locking on done
      done <- liftIO $ takeMVar doneM
      liftIO $ putMVar doneM (S.insert a done)

      -- Keep track of the collaborations we found, locking
      collaborations <- liftIO $ takeMVar collaborationsM
      liftIO $ putMVar collaborationsM (collaborations SQ.>< (SQ.fromList cs))

      -- Get collaborators that we haven't already scraped
      -- Add all new artists to the seen set so that the queue doesn't have duplicates
      seen <- liftIO $ takeMVar seenM
      let newAs = nub $ filter (not . (`S.member` seen)) (concatMap getCollaborators cs)
      liftIO $ putMVar seenM (foldl' (flip S.insert) seen newAs)

      -- Add collaborators to queue for scraping, locking the queue again
      q <- liftIO $ takeMVar qM
      liftIO $ putMVar qM (q SQ.>< (SQ.fromList newAs))
    SQ.EmptyL -> do
      -- Still need to reinstate queue state here
      liftIO $ putMVar qM q

      return ()

logScraper :: StateT Scraper IO ()
logScraper = do
  q <- gets (view queue) >>= (liftIO . readMVar)
  cs <- gets (view collaborations) >>= (liftIO . readMVar)
  done <- gets (view done) >>= (liftIO . readMVar)
  case SQ.viewl q of
    (Artist _ (ArtistName name)) SQ.:< _ -> liftIO $ putStrLn $ "Next Artist: " <> show name
    SQ.EmptyL -> liftIO $ putStrLn "No next artist"
  liftIO $ putStrLn $ "Queue length: " <> show (SQ.length q)
  liftIO $ putStrLn $ "Collaborations found: " <> show (SQ.length cs)
  liftIO $ putStrLn $ "Artists scraped: " <> show (S.size done)

runScraper :: StateT Scraper IO ()
runScraper = do
  stepScraper
  -- Artificial delay to avoid getting so rate-limited
  -- liftIO $ threadDelay 200000
  runScraper

scraperLogger :: Scraper -> IO ()
scraperLogger scraper = do
  evalStateT logScraper scraper
  threadDelay 1000000
  scraperLogger scraper

main :: IO ()
main = do
  -- Get OAuth token
  secret <- clientSecret
  token <- getToken secret

  -- Define seed artist and scraper state
  let jam = Artist (ArtistId "6ST2sHlQoWYjxkIVnuW2mr") (ArtistName "Jam Baxter")
  queueM <- newMVar $ SQ.singleton jam
  collaborationsM <- newMVar SQ.empty
  doneM <- newMVar S.empty
  seenM <- newMVar S.empty
  let scraper = Scraper { _queue = queueM
                        , _collaborations = collaborationsM
                        , _done = doneM
                        , _seen = seenM
                        , _token = token
                        }

  -- Kick off N workers
  replicateM_ 8 $ forkIO $ evalStateT runScraper scraper

  -- Block on the logger thread
  scraperLogger scraper

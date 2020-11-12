{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}


module Main where

import Data.List ( foldl', nub )
import Data.Maybe ( catMaybes, fromMaybe )
import GHC.Generics (Generic)
import Data.Graph
import Data.Array
import System.IO.Unsafe
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Concurrent.MVar
    ( newMVar, putMVar, readMVar, takeMVar, MVar )
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding  as LE
import qualified Data.Text.Encoding  as E
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBC
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad ( replicateM_, forM_ )
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
                       , _conn :: Connection
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

      -- Now handle database writes before moving on in key-satisfying order:
      -- Need to write the artist we just got and all other new artists
      let artistToTuple (Artist (ArtistId aId) (ArtistName aName)) = (aId, aName)
      _ <- liftIO
        $ executeMany (s ^. conn) "insert into artist (artist_id, artist_name) values (?, ?) on conflict do nothing"
        $ (nub $ artistToTuple <$> (a:newAs))

      -- Then the tracks
      let collaborationToTrackTuple (Collaboration _ _ (Track (TrackId tId) (TrackName tName) _)) = (tId, tName)
      _ <- liftIO $ executeMany (s ^. conn) "insert into track (track_id, track_name) values (?, ?) on conflict do nothing"
        $ (nub $ collaborationToTrackTuple <$> cs)

      -- Then the collaborations
      let collaborationToTuple (Collaboration (Artist (ArtistId aId1) _) (Artist (ArtistId aId2) _) (Track (TrackId tId) _ _)) = (aId1, aId2, tId)
      _ <- liftIO $ executeMany (s ^. conn) "insert into collaboration (first_artist_id, second_artist_id, track_id) values (?, ?, ?) on conflict do nothing"
        $ (nub $ collaborationToTuple <$> cs)
      return ()
    SQ.EmptyL -> do
      -- Still need to reinstate queue state here
      liftIO $ putMVar qM q
      return ()

logScraper :: StateT Scraper IO ()
logScraper = do
  q <- gets (view queue) >>= (liftIO . readMVar)
  cs <- gets (view collaborations) >>= (liftIO . readMVar)
  done <- gets (view done) >>= (liftIO . readMVar)
  seen <- gets (view seen) >>= (liftIO . readMVar)
  case SQ.viewl q of
    (Artist _ (ArtistName name)) SQ.:< _ -> liftIO $ putStrLn $ "Next Artist: " <> show name
    SQ.EmptyL -> liftIO $ putStrLn "No next artist"
  liftIO $ putStrLn $ "Queue length: " <> show (SQ.length q)
  liftIO $ putStrLn $ "Collaborations found: " <> show (SQ.length cs)
  liftIO $ putStrLn $ "Artists seen: " <> show (S.size seen)
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

scraperMain :: IO ()
scraperMain = do
  -- DB init
  conn <- connect defaultConnectInfo { connectDatabase = "featchains"
                                     , connectPassword = "postgres"
                                     }

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
                        , _conn = conn
                        }

  -- Kick off N workers
  replicateM_ 8 $ forkIO $ evalStateT runScraper scraper

  -- Block on the logger thread
  scraperLogger scraper

data GraphInfo = GraphInfo { _graph :: Graph
                           , _idToArtist :: M.Map T.Text T.Text
                           , _artistToId :: M.Map T.Text T.Text
                           , _idToTrack :: M.Map T.Text T.Text
                           , _nodeFromVertex :: Vertex -> (T.Text, T.Text, [T.Text])
                           , _vertexFromKey :: T.Text -> Maybe Vertex
                           , _edgeLookup :: M.Map (T.Text, T.Text) [T.Text]
                           }
makeLenses ''GraphInfo

buildGraph :: Connection -> IO GraphInfo
buildGraph conn = do
  -- Some convenience lookups
  idToArtist <-
    M.fromList
    <$> (query_ conn [sql| select artist_id, artist_name from artist|] :: (IO [(T.Text, T.Text)]))
  let artistToId = M.fromList $ (\(k, v) -> (v, k)) <$> M.toList idToArtist
  idToTrack <-
    M.fromList
    <$> (query_ conn [sql| select track_id, track_name from track|] :: (IO [(T.Text, T.Text)]))

  print "Querying..."
  rows <-
    query_ conn
    $ [sql| select
              first_artist_id,
              second_artist_id,
              track_id
            from collaboration
      |] :: (IO [(T.Text, T.Text, T.Text)])

  print $ T.pack (show (M.size idToArtist)) <> " artists"
  print $ T.pack (show (M.size idToTrack)) <> " tracks"
  print $ T.pack (show $ length rows) <> " edges"

  print "Building graph and lookups"

  -- Have a way to "label" vertices by the list of tracks that link the artists
  let rowToEdgeLookup (aId1, aId2, tId) = ((aId1, aId2), [tId])
      edgeLookup = M.fromListWith (++) $ rowToEdgeLookup <$> rows

  -- Convert into graph form with nodes being names, IDs being IDs
  -- Need to get a lookup map from ID to all things it's connected to
  let rowToEdges (aId1, aId2, _) = [ (aId1, [aId2]) ]
      adjacencyMap = M.fromListWith (++) (concatMap rowToEdges rows)
      edgeList = (\(aId1, toIds) -> (aId1, aId1, nub toIds)) <$> M.toList adjacencyMap
      (graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList

  return $ GraphInfo { _graph = graph
                     , _idToArtist = idToArtist
                     , _artistToId = artistToId
                     , _idToTrack = idToTrack
                     , _nodeFromVertex = nodeFromVertex
                     , _vertexFromKey = vertexFromKey
                     , _edgeLookup = edgeLookup
                     }

-- TODO: Make safe
getPath :: T.Text -> T.Text -> GraphInfo -> Maybe [(ArtistName, ArtistName, [TrackName])]
getPath a1Name a2Name g = do
  let fst3 (a, _, _) = a
      vToId = fst3 . (g^.nodeFromVertex)
      vToName = (flip M.lookup $ (g^.idToArtist)) . fst3 . (g^.nodeFromVertex)
      (Just a1Id) = M.lookup a1Name (g^.artistToId)
      (Just a2Id) = M.lookup a2Name (g^.artistToId)
      (Just a1V) = (g^.vertexFromKey) a1Id
      (Just a2V) = (g^.vertexFromKey) a2Id
  case graphBfs (g^.graph) a1V a2V of
    Nothing -> Nothing
    Just path -> do
      let aIdPath = vToId <$> path
          aIdPairs = zip aIdPath $ tail aIdPath
          tIdPath = catMaybes $ (flip M.lookup (g^.edgeLookup)) <$> aIdPairs
          tNamePath :: [[T.Text]]
          tNamePath = (fmap.fmap) (fromMaybe "" . flip M.lookup (g^.idToTrack)) tIdPath
          aNamePath = catMaybes $ (flip M.lookup (g^.idToArtist)) <$> aIdPath
          aNamePairs :: [(T.Text, T.Text)]
          aNamePairs = zip aNamePath $ tail aNamePath
          nameTrackPath = (\((a, b), c ) -> (ArtistName a, ArtistName b, TrackName <$> c)) <$> (zip aNamePairs tNamePath)
      Just nameTrackPath

getGraph :: IO GraphInfo
getGraph = do
  conn <- connect defaultConnectInfo { connectDatabase = "featchains"
                                     , connectPassword = "postgres"
                                     }
  buildGraph conn

unsafeGetGraph :: GraphInfo
unsafeGetGraph = unsafePerformIO getGraph

printPath :: [(ArtistName, ArtistName, [TrackName])] -> IO ()
printPath path = do
  forM_ path (\((ArtistName n1), (ArtistName (n2)), ts) -> do
                 putStrLn $ T.unpack $ n1 <> " and " <> n2 <> " collaborated on: "
                 forM_ ts (\(TrackName tn) -> putStrLn $ T.unpack tn)
                 putStrLn "")

-- Prettyprinter for console
pp :: GraphInfo -> String -> String -> IO ()
pp g a1 a2 = do
  let path = getPath (T.pack a1) (T.pack a2) g
  case path of
    Just p -> printPath p
    Nothing -> putStrLn "No path"

-- BFS between two nodes, returns the path if there is one
graphBfs :: Graph -> Vertex -> Vertex -> Maybe [Vertex]
graphBfs graph start end = go graph end S.empty (SQ.singleton (start, []))
  where
    go graph end visited q =
      case SQ.viewl q of
        ((current, path) SQ.:< queue) ->
          let newVisited = S.insert current visited
              neighbours = filter (not . (`S.member` newVisited)) (graph ! current)
              newPath = current:path
              newQueue = queue SQ.>< SQ.fromList ((,newPath) <$> neighbours)
           in if current == end
              then Just $ reverse (current:path)
              else go graph end newVisited newQueue
        SQ.EmptyL -> Nothing

main :: IO ()
main = do
  print "main disabled"
  --scraperMain

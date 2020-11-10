{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text(..))

{- TODO
Example API call
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

clientId :: Text
clientId = "28342f3712b74711af0f925204a8aae1"

clientSecret :: IO Text
clientSecret = do
  content <- readFile "secret"
  return $ T.pack $ head (lines content)

main :: IO ()
main = do
  secret <- clientSecret
  print secret

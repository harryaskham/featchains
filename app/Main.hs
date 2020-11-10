{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib ( someFunc )
import qualified Data.Text as T
import Data.Text (Text(..))

clientId :: T.Text
clientId = "28342f3712b74711af0f925204a8aae1"

clientSecret :: IO Text
clientSecret = do
  content <- readFile "secret"
  return $ T.pack $ head (lines content)

main :: IO ()
main = do
  secret <- clientSecret
  print secret

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib ( someFunc )
import qualified Data.Text as T
import Data.Text (Text(..))

clientId :: T.Text
clientId = "28342f3712b74711af0f925204a8aae1"

clientSecret :: Text
clientSecret = "fa95fe45a19d4105b3fe16ae8c95561a"

main :: IO ()
main = someFunc

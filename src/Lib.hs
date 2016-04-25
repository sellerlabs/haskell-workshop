{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.String.Conversions
-- Part 2
import Data.List (transpose)
import Control.Concurrent.Async (mapConcurrently)

data Post = Post
  { subreddit :: T.Text
  , author :: T.Text
  , score :: Int
  , url :: T.Text
  , title :: T.Text
  } deriving (Show)

data Listing = Listing { posts :: [Post] }
  deriving (Show)

getReddit :: String -> IO Listing
getReddit subreddit = do
  let url = "http://www.reddit.com/r/" <> subreddit <> "/hot/.json?count=25"
  response <- simpleHTTP (getRequest url)
  body <- getResponseBody response
  case eitherDecode (cs body) of
    Right listing -> pure listing
    Left e -> error e

-- Part 2

getReddits :: [String] -> IO Listing
getReddits reddits = do
  listings <- mapConcurrently getReddit reddits
  pure (mergeListings listings)

-- Technically can be any Foldable but this suffices for now.
-- TODO: Simplify?
mergeListings :: [Listing] -> Listing
mergeListings listings = Listing (concat . transpose $ map posts listings)

someFunc :: IO ()
someFunc = getReddits ["haskell", "darksouls"] >>= print

-- INTERNALS

instance FromJSON Post where
  parseJSON = withObject "post" $ \json -> do
    dataO <- json .: "data"
    Post
      <$> dataO .: "subreddit"
      <*> dataO .: "author"
      <*> dataO .: "score"
      <*> dataO .: "url"
      <*> dataO .: "title"

instance FromJSON Listing where
  parseJSON = withObject "listing" $ \json -> do
    dataO <- json .: "data"
    children <- dataO .: "children"
    posts_ <- withArray "children" (mapM parseJSON . V.toList) children
    pure (Listing posts_)

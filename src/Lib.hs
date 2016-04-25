{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
-- Part 3
import Control.Monad.IO.Class
import Data.List.Split
import Data.Monoid
import Web.Spock.Safe

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
mergeListings listings = Listing (interleaveMany (map posts listings))
  where interleaveMany = concat . transpose

someFunc :: IO ()
someFunc = getReddits ["haskell", "darksouls"] >>= print

-- Part 3 (Server)

server :: IO ()
server =
    runSpock 8080 $ spockT id $
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           text ("Hello " <> name <> "!")
       get "reddit" $ do
          Just reddits <- param "reddits"
          listing <- liftIO $ getReddits (splitOn "," reddits)
          text (T.pack (show listing))

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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE EmptyDataDecls #-}

module MyReddit
    ( server
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
import Lucid
import Lucid.Bootstrap

-- Part 1

data Post = Post
  { subreddit :: T.Text
  , author :: T.Text
  , score :: Int
  , url :: T.Text
  , title :: T.Text
  , thumbnail :: T.Text
  } deriving (Show)

data Listing = Listing { posts :: [Post] }
  deriving (Show)

getReddit :: String -> IO Listing
getReddit subreddit = error "Not implemented...yet!"

-- Part 2

getReddits :: [String] -> IO Listing
getReddits reddits = error "Not implemented...yet!"

mergeListings :: [Listing] -> Listing
mergeListings listings = error "Not implemented...yet!"

printReddits :: [String] -> IO ()
printReddits reddits = error "Not implemented...yet!"

-- Part 3 (Server)

server :: IO ()
server = error "Not implemented...yet!"

bootstrap :: Html ()
bootstrap = error "Not implemented...yet!"

viewListing :: Listing -> Html ()
viewListing = error "Not implemented...yet!"

renderPost :: Post -> Html ()
renderPost = error "Not implemented...yet!"

renderThumbnail :: T.Text -> Html ()
renderThumbnail src = error "Not implemented...yet!"

colMd :: Int -> Html () -> Html ()
colMd span = error "Not implemented...yet!"

-- INTERNALS
instance FromJSON Post where
  parseJSON = withObject "post" $ \json -> do
    dataO <- json .: "data"
    subreddit_ <-  dataO .: "subreddit"
    author_ <- dataO .: "author"
    score_ <- dataO .: "score"
    url_ <- dataO .: "url"
    title_ <- dataO .: "title"
    thumbnail_ <- dataO .: "thumbnail"
    pure (Post subreddit_ author_ score_ url_ title_ thumbnail_)

instance FromJSON Listing where
  parseJSON = withObject "listing" $ \json -> do
    dataO <- json .: "data"
    children <- dataO .: "children"
    posts_ <- withArray "children" (mapM parseJSON . V.toList) children
    pure (Listing posts_)

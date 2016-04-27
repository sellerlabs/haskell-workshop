{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Reddit
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
mergeListings :: [Listing] -> Listing
mergeListings listings = Listing (interleaveMany (map posts listings))
  where interleaveMany = concat . transpose

printReddits :: IO ()
printReddits = do
  listing <- getReddits ["haskell", "darksouls"]
  print listing

-- Part 3 (Server)

server :: IO ()
server = runSpock 8080 $ spockT id $
  do get "reddit" $ do
      reddits <- param "reddits"
      case reddits of
        Nothing -> text "No Reddits Provided"
        Just reddits' -> do
          let redditList = splitOn "," reddits'
          listing <- liftIO (getReddits redditList)
          liftIO (print listing)
          html (cs (renderText (viewAll listing)))

bootstrap :: Html ()
bootstrap = link_
  [ href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  , rel_ "stylesheet"
  ]

viewAll :: Listing -> Html ()
viewAll listing = do
  bootstrap
  container_ (viewListing listing)

viewListing :: Listing -> Html ()
viewListing (Listing posts) =
  mconcat (map renderPost posts)

renderPost :: Post -> Html ()
renderPost (Post subreddit_ author_ score_ url_ title_ thumbnail_) = do
  row_ $ do
    renderThumbnail thumbnail_
    colMd 8 (a_ [href_  url_] (toHtml title_))
  row_ $ do
    colMd 2 ("Score: " <> toHtml (show score_))
    colMd 2 (toHtml subreddit_)
    colMd 2 (toHtml author_)
  br_ []

renderThumbnail :: T.Text -> Html ()
renderThumbnail src = do
  let imageColumn = colMd 2 (img_ [src_ src, width_ "80px"])
  case T.breakOn "://" src of
    ("http", _) -> imageColumn
    ("https", _) -> imageColumn
    _ -> div_ ""

colMd :: Int -> Html () -> Html ()
colMd span = div_ [class_ ("col-md-" <> cs (show span))]

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

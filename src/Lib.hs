{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

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
import qualified Data.Text.Lazy as TL
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
          reddits <- param "reddits"
          case reddits of
            Nothing -> text "No Reddits Provided"
            Just reddits' -> do
              listing <- liftIO $ getReddits (splitOn "," reddits')
              liftIO $ print listing
              html (TL.toStrict (renderText (viewAll listing)))

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
  mconcat $ map (renderPost) posts

renderPost :: Post -> Html ()
renderPost (Post{..}) = do
  row_ $ do
    renderThumbnail thumbnail
    colMd 8 (a_ [href_  url] (toHtml title))
  row_ $ do
    colMd 2 (toHtml $ "Score: " <> T.pack (show score))
    colMd 2 (toHtml subreddit)
    colMd 2 (toHtml author)
  br_ []

renderThumbnail :: T.Text -> Html ()
renderThumbnail src =
  case T.breakOn "://" src of
    (a, _) | a == "http" || a == "https" ->
      colMd 2 (img_ [src_ src, width_ "80px"])
    _ -> div_ ""

colMd :: Int -> Html () -> Html ()
colMd rows = div_ [class_ ("col-md-" <> T.pack (show rows))]

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
      <*> dataO .: "thumbnail"

instance FromJSON Listing where
  parseJSON = withObject "listing" $ \json -> do
    dataO <- json .: "data"
    children <- dataO .: "children"
    posts_ <- withArray "children" (mapM parseJSON . V.toList) children
    pure (Listing posts_)

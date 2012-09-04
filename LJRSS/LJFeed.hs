module LJRSS.LJFeed where

import LJRSS.LJEntry
import LJRSS.TimeFormat

import Data.Time

import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax

data TLJFeed = LJFeed {
  ljUsername :: String,
  ljFeedLastUpdate :: LocalTime,
  ljFeedItems :: [TLJEntry]
} deriving (Show)

parseLJFeed :: Bool -> String -> Maybe TLJFeed
parseLJFeed skipEntries src = do 
  (RSSFeed rssFeedData) <- parseFeedString src
  let rssChannelData = rssChannel rssFeedData
  pubDate <- rssLastUpdate rssChannelData >>= parseLJTime
  return . LJFeed "" pubDate $ case skipEntries of
                                  False -> map fromRSSItem $ rssItems rssChannelData
                                  True -> []

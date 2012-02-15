module LJRSS.LJFeed where

import LJRSS.LJEntry
import LJRSS.TimeFormat

import Data.Time

import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax

data TLJFeed = LJFeed {
  ljFeedLastUpdate :: LocalTime,
  ljFeedItems :: [TLJEntry]
} deriving (Show)

parseLJFeed :: String -> Maybe TLJFeed
parseLJFeed src = do 
  (RSSFeed rssFeedData) <- parseFeedString src
  let rssChannelData = rssChannel rssFeedData
  pubDate <- rssLastUpdate rssChannelData >>= parseLJTime
  return . LJFeed pubDate . map fromRSSItem $ rssItems rssChannelData

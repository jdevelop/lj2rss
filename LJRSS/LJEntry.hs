module LJRSS.LJEntry where

import LJRSS.TimeFormat

import Data.Int
import Data.Time
import Data.Maybe
import System.Locale
import Text.RSS.Syntax

data TLJEntry = Invalid | LJEntry {
  ljEntryLink :: String,
  ljEntryPubDate :: LocalTime,
  ljEntryText :: String
} deriving (Show)

fromRSSItem :: RSSItem -> TLJEntry
fromRSSItem src = fromMaybe Invalid $ do
  entryLink <- rssItemLink src
  pubDate <- rssItemPubDate src >>= parseLJTime
  entryText <- rssItemDescription src
  return $ LJEntry entryLink pubDate entryText

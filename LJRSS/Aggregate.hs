{-# LANGUAGE BangPatterns #-}
module LJRSS.Aggregate where

import LJRSS.LJTransport
import LJRSS.LJFeed
import LJRSS.LJEntry
import LJRSS.LJConfig
import LJRSS.Friends
import Control.Applicative ((<$>))
import Control.Arrow (arr, first, second, (***))

import Control.Monad

import Data.Time
import qualified Data.Map as DM
import Data.Maybe

import Network.Curl as C

readNewEntries :: TLJConfig -> String -> IO (Maybe TLJFeed)
readNewEntries cfg@(LJConfig username password lastDateCache) ljFriend = 
  liftM ( (getNewEntries <$>) . parseLJFeed) $ getFeedContent username password feedUrl
  where
    lastReadTime = DM.lookup ljFriend lastDateCache
    feedUrl = "http://" ++ ljFriend ++ ".livejournal.com/data/rss?auth=digest"
    getNewEntries feed@(LJFeed currentFeedTime entries) = maybe feed f lastReadTime
      where 
        f lastReadTime' = LJFeed currentFeedTime $ filter ( (> lastReadTime') . ljEntryPubDate ) entries

aggregateEntries :: TLJConfig -> IO (TLJConfig, [TLJFeed])
aggregateEntries cfg@(LJConfig username password lastDateCache) = do
 curl <- C.initialize
 !friendList <- getLJFriends username
 g friendList <$> mapM ( (readNewEntries cfg . getUsername) ) friendList
 where
  g friendList !items = go . map ( second (arr fromJust) ) . filter (isJust . snd) $ zip friendList items
  go !items = first (arr ( \x -> cfg { sessions = x } ) ) $ foldr f (lastDateCache, []) items
  f (uName, feed@(LJFeed feedUpdateTime _)) = 
    arr (DM.insert (getUsername uName) feedUpdateTime ) *** arr ( feed : )

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

readNewEntries :: Curl -> TLJConfig -> String -> IO (Maybe TLJFeed)
readNewEntries curl cfg@(LJConfig username password lastDateCache) ljFriend = 
  liftM ( (getNewEntries <$>) . parseLJFeed) $ getFeedContent curl username password feedUrl
  where
    lastReadTime = DM.lookup ljFriend lastDateCache
    feedUrl = "http://" ++ ljFriend ++ ".livejournal.com/data/rss?auth=digest"
    getNewEntries feed@(LJFeed currentFeedTime entries) = maybe feed f lastReadTime
      where 
        f lastReadTime' = LJFeed currentFeedTime $ filter ( (> lastReadTime') . ljEntryPubDate ) entries

aggregateEntries :: TLJConfig -> IO (TLJConfig, [TLJFeed])
aggregateEntries cfg@(LJConfig username password lastDateCache) = do
 curl <- C.initialize
 friendList <- getLJFriends curl username
 feeds <- mapM ( readNewEntries curl cfg . getUsername ) friendList
 return $ go . map ( second (arr fromJust) ) . filter (isJust . snd) $ zip friendList feeds
 where
  go = first (arr ( \x -> cfg { sessions = x } ) ) . foldr f (lastDateCache, [])
  f (uName, feed@(LJFeed feedUpdateTime _)) = 
    arr (DM.insert (getUsername uName) feedUpdateTime ) *** arr ( feed : )

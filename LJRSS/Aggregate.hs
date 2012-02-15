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
import Control.Monad.Error

import Data.Time
import qualified Data.Map as DM
import Data.Maybe

import Network.Curl as C

data TLJFeedAggregatorError = LJFeedTransportError TLJTransportError | LJFeedNotice String

instance Error TLJFeedAggregatorError where
 strMsg = LJFeedNotice

instance Show TLJFeedAggregatorError where
  show (LJFeedTransportError e) = show e
  show (LJFeedNotice s) = s

type LJFeedAggregatorMonad = ErrorT TLJFeedAggregatorError IO

readNewEntries :: TLJConfig -> String -> LJFeedAggregatorMonad (Maybe TLJFeed)
readNewEntries cfg@(LJConfig username password lastDateCache) ljFriend = do
  content <- liftIO . runErrorT $ getFeedContent username password feedUrl
  case content of
    Left fatalTransportError@(LJTransportFatal _) -> throwError $ LJFeedTransportError fatalTransportError
    Right content -> (return $!) $ ((getNewEntries <$>) . parseLJFeed)  content
    otherwise -> return Nothing
  where
    lastReadTime = DM.lookup ljFriend lastDateCache
    feedUrl | elem '_' ljFriend = "http://www.livejournal.com/users/" ++ ljFriend
            | otherwise = "http://" ++ ljFriend ++ ".livejournal.com/data/rss?auth=digest"
    getNewEntries feed@(LJFeed currentFeedTime entries) = maybe feed f lastReadTime
      where
        f lastReadTime' = LJFeed currentFeedTime $ filter ( (> lastReadTime') . ljEntryPubDate ) entries

aggregateEntries :: TLJConfig -> LJFeedAggregatorMonad (TLJConfig, [TLJFeed])
aggregateEntries cfg@(LJConfig username password lastDateCache) = do
 curl <- liftIO C.initialize
 !friendList <- liftIO $ getLJFriends username
 g friendList <$> mapM ( (readNewEntries cfg . getUsername) ) friendList
 where
  g friendList !items = go . map ( second (arr fromJust) ) . filter (isJust . snd) $ zip friendList items
  go !items = first (arr ( \x -> cfg { sessions = x } ) ) $ foldr f (lastDateCache, []) items
  f (uName, feed@(LJFeed feedUpdateTime _)) = 
    arr (DM.insert (getUsername uName) feedUpdateTime ) *** arr ( feed : )

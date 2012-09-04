{-# LANGUAGE BangPatterns #-}
module LJRSS.Agregate where

import LJRSS.LJTransport
import LJRSS.LJFeed
import LJRSS.LJEntry
import qualified LJRSS.LJConfig as LJC
import LJRSS.LJFriends
import Control.Applicative ((<$>))
import Control.Arrow (arr, first, second, (***))

import Control.Monad.Error

import Data.Time
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe

import Network.Curl as C

import Debug.Trace

data TLJFeedAggregatorError = LJFeedTransportError TLJTransportError | 
                              LJFeedRetryFailed TLJTransportError | 
                              LJFeedNotice String

instance Error TLJFeedAggregatorError where
 strMsg = LJFeedNotice

instance Show TLJFeedAggregatorError where
  show (LJFeedTransportError e) = show e
  show (LJFeedNotice s) = s

type LJFeedAggregatorMonad = ErrorT TLJFeedAggregatorError IO

readNewEntries :: LJC.TLJConfig -> 
                  Int -> 
                  Maybe TLJTransportError -> 
                  String -> 
                  LJFeedAggregatorMonad (Maybe TLJFeed)
readNewEntries _ 0 (Just lastError) _ = throwError $ LJFeedRetryFailed lastError
readNewEntries _ 0 Nothing _ = throwError $ LJFeedNotice "Wrong configuration - no retry count provided"
readNewEntries cfg retries lastError ljFriend = do
  content <- liftIO . runErrorT $ getFeedContent username password feedUrl
  case content of
    Left fatalTransportError@(LJTransportFatal _) -> throwError $ LJFeedTransportError fatalTransportError
    Left retryTransportError@(LJTransportRetryable _) -> readNewEntries cfg (pred retries) (Just retryTransportError) ljFriend
    Right content -> (return $!) $ ((getNewEntries <$>) . ( parseLJFeed ( LJC.noEntries cfg ) ))  content
    otherwise -> return Nothing
  where
    username = LJC.username cfg
    password = LJC.password cfg
    lastDateMap = LJC.sessions cfg
    lastReadTime = DM.lookup ljFriend lastDateMap
    feedUrl | elem '_' ljFriend = "http://www.livejournal.com/users/" ++ ljFriend ++ "?auth=digest"
            | otherwise = "http://" ++ ljFriend ++ ".livejournal.com/data/rss?auth=digest"
    getNewEntries feed@(LJFeed _ currentFeedTime entries) = maybe (feed {ljUsername = ljFriend }) f lastReadTime
      where
        f lastReadTime' = LJFeed ljFriend currentFeedTime $ filter (filterEntries lastReadTime') entries
        filterEntries lt Invalid = traceShow "Invalid entry detected" $ False
        filterEntries lt entry = (> lt) $ ljEntryPubDate entry

aggregateEntries :: LJC.TLJConfig -> FriendsTransformer -> LJFeedAggregatorMonad (LJC.TLJConfig, [TLJFeed])
aggregateEntries cfg ft = do
  liftIO C.initialize
  friendList <- liftIO $ (ft <$> getLJFriends username)
  g friendList <$> mapM ( (readNewEntries cfg retries Nothing . getUsername) ) friendList
  where
    g friendList !items = go . map ( second (arr fromJust) ) . filter (isJust . snd) $ zip friendList items
    go !items = first (arr ( \x -> cfg { LJC.sessions = x } ) ) $ foldr f (lastDateMap, []) items
    f (uName, feed@(LJFeed _ feedUpdateTime _)) = 
      arr (DM.insert (getUsername uName) feedUpdateTime ) *** arr ( feed : )
    username = LJC.username cfg
    password = LJC.password cfg
    lastDateMap = LJC.sessions cfg
    retries = LJC.retryBeforeFail cfg

aggregateEntriesDefaultExclude :: LJC.TLJConfig -> LJFeedAggregatorMonad (LJC.TLJConfig, [TLJFeed])
aggregateEntriesDefaultExclude cfg = aggregateEntries cfg (filter (not . flip DS.member ignored . getUsername)) 
  where
    ignored = LJC.ignored cfg

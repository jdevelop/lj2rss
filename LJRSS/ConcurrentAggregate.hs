module LJRSS.ConcurrentAggregate where

import qualified Data.Map as DM
import qualified Data.Set as DS

import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Applicative ((<$>))

import qualified Network.Curl as C

import LJRSS.LJFeed
import qualified LJRSS.Aggregate as LJA
import qualified LJRSS.LJConfig as LJC
import LJRSS.Email
import LJRSS.LJFriends

data TLJFeedInput = Poison | FeedInputStr String

aggregateEntries :: Int -> LJC.TLJConfig -> FriendsTransformer -> IO LJC.TLJConfig
aggregateEntries numThreads cfg friendFilter = do
  friendList <- (friendFilter <$> getLJFriends username)
  configMVar <- newMVar currentSessions
  finishMVar <- newEmptyMVar
  inputMVar <- newEmptyMVar
  latchMVar <- newMVar numThreads
  C.initialize
  mapM_ ( const $ forkIO (fetchFeed inputMVar configMVar latchMVar)) [1..numThreads]
  forkIO $ do
    mapM_ (putMVar inputMVar . FeedInputStr . getUsername) friendList 
    putMVar inputMVar Poison
  countdownLatch numThreads latchMVar
  newSesions <- takeMVar configMVar
  return $ cfg { LJC.sessions = newSesions }
  where
    username = LJC.username cfg
    retries = LJC.retryBeforeFail cfg
    currentSessions = LJC.sessions cfg
    sendFrom = LJC.notificationFromAddress cfg
    sendTo = LJC.notificationAddress cfg
    countdownLatch sleepTime latchMVar = do
      currentThreads <- takeMVar latchMVar
      if currentThreads <= 0
        then return ()
        else putMVar latchMVar currentThreads
      threadDelay sleepTime
      countdownLatch sleepTime latchMVar
    fetchFeed inputMVar configMVar latchMVar = do
      friendName <- takeMVar inputMVar
      case friendName of
        Poison -> do 
          liftIO $ do
            modifyMVar_ latchMVar ( return . pred  )
            putMVar inputMVar Poison
          return ()
        FeedInputStr name -> do 
          runErrorT $ goWithFeed configMVar name 
          fetchFeed inputMVar configMVar latchMVar
    goWithFeed configMVar name = do
      feedData <- LJA.readNewEntries cfg retries Nothing name
      case feedData of
        Nothing -> return ()
        Just (LJFeed _ lastUpdateTime entries) -> liftIO $ do
          mapM (sendMail sendFrom sendTo name) entries
          liftIO $ modifyMVar_ configMVar (updateConfig name lastUpdateTime)
          return ()
    updateConfig name lastUpdateTime newCfg = return $ DM.insert name lastUpdateTime newCfg


aggregateEntriesDefaultExclude :: Int -> LJC.TLJConfig -> IO (LJC.TLJConfig)
aggregateEntriesDefaultExclude numThreads cfg = aggregateEntries numThreads cfg (filter (not . flip DS.member ignored . getUsername)) 
  where
    ignored = LJC.ignored cfg

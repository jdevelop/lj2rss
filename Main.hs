module Main where

import LJRSS.Aggregate
import LJRSS.LJConfig
import LJRSS.LJFeed
import LJRSS.Email

import qualified Data.Map as DM

import Control.Monad.Error (runErrorT)

main = do 
  currentCfg <- readConfig
  result <- runErrorT $ aggregateEntriesDefaultExclude currentCfg
  case result of
    Left err -> print err
    Right vals -> notify currentCfg vals
  putStrLn "Done"
  where
    notify currentCfg (cfg, feeds) = do 
      mapM_ (send cfg) $ concatMap (\(LJFeed username _ feedItems) -> map ( (,) username ) feedItems ) feeds
      writeConfig cfg
    f timeLeft timeRight | timeLeft == timeRight = Nothing
                         | otherwise = Just timeRight
    send cfg (journal, ljEntry) = 
      let fromAddr = notificationFromAddress cfg
          toAddr = notificationAddress cfg
      in do 
          sendMail fromAddr toAddr journal ljEntry

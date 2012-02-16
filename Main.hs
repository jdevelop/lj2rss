module Main where

import LJRSS.Aggregate
import LJRSS.LJConfig
import qualified Data.Map as DM

import Control.Monad.Error (runErrorT)

main = do 
  currentCfg <- readConfig
  result <- runErrorT $ aggregateEntriesDefaultExclude currentCfg
  case result of
    Left err -> print err
    Right vals -> g currentCfg vals
  putStrLn "Done"
  where
    g currentCfg (cfg, feeds) = do 
      print $ DM.differenceWith f (sessions currentCfg) (sessions cfg)
      writeConfig cfg
    f timeLeft timeRight | timeLeft == timeRight = Nothing
                         | otherwise = Just timeRight

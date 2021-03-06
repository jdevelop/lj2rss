{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import LJRSS.ConcurrentAgregate
import LJRSS.LJConfig
import LJRSS.LJFeed
import LJRSS.Email
import System.Console.GetOpt

import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Control.Monad.Error (runErrorT, liftIO)
import Control.Monad (liftM)
import Control.Applicative ((<$>))

import System.Environment (getArgs)
import System.IO

data CMDOptions = Init | Update | Snapshot | Threads Int | UpdateUser String deriving (Show)

options :: [OptDescr CMDOptions]
options = [ Option ['i'] ["init"] (NoArg Init) "Initialize settings",
            Option ['r'] ["refresh"] (NoArg Update) "Update all friends",
            Option ['s'] ["snapshot"] (NoArg Snapshot) "Take snapshot of last post dates - no mail",
            Option ['t'] ["threads"] (ReqArg (Threads . read) "5") "Number of threads",
            Option ['u'] ["update"] (OptArg (UpdateUser . fromMaybe "") "USERNAME") "Update (or add) single journal" 
          ]

main = do 
  parsedOption <- validate =<< liftM (getOpt Permute options) getArgs
  go parsedOption
  where
    validate ([],_,_) = failWithError ["No options provided"] 
    validate (opts,_,[]) = return opts
    validate (_,_,errs) = failWithError $ "Parse error " : errs
    failWithError errs = fail $ concat errs ++ usageInfo "Usage: ljrss [OPTION]" options


go [Init] = do
  putStrLn "Please provide LiveJournal username"
  username <- getLine
  password <- readPassword
  skipList <- ignored
  putStrLn "Please provide target email address to be used for sending notifications"
  recipient <- getLine
  putStrLn "Please provide sender email address to use in From: field of notifications"
  sender <- getLine
  putStrLn "Do the following settings look ok?"
  putStrLn $ formatTextL "Username" 12 ++ username
  putStrLn $ formatTextL "From" 12 ++ sender
  putStrLn $ formatTextL "To" 12 ++ recipient
  putStrLn $ formatTextL "Ignored" 12 ++ (show $ DS.toList skipList)
  putStrLn "[Y]es or [N]o?"
  yesNo <- getLine
  if "Y" /= yesNo && "y" /= yesNo
    then do 
      go [Init]
    else do 
      writeConfig $ LJConfig username password recipient sender skipList 10 5 DM.empty True
      putStrLn "Configuration created, now use 'ljrss -r' to read friend feed"
  where
    formatTextL src len = T.unpack . T.justifyLeft len ' ' $ T.pack src
    ignored = do
      putStrLn "Do you want to setup ignored users (Y/N)?"
      answer <- getLine
      if answer /= "Y" && answer /= "y"
      then do
        return DS.empty
      else do
        putStrLn "Provide usernames line by line, empty line completes list"
        readIgnored
    readIgnored = do
      username <- getLine
      if null username
      then
        return DS.empty
      else 
        (DS.insert username ) <$> readIgnored
    readPassword = do
      hSetEcho stdout False
      putStrLn "Please provide LiveJournal password"
      password <- getLine
      putStrLn "Please confirm LiveJournal password"
      confirmPassword <- getLine
      hSetEcho stdout True
      if (password /= confirmPassword)
      then do
        putStrLn "\nPasswords mismatch, please try again \n"
        readPassword
      else
        return password


go [Update, (Threads numThreads)] = do
  currentCfg <- readConfig 
  newConfig <- aggregateEntriesDefaultExclude numThreads currentCfg 
  writeConfig $ newConfig { noEntries = False }

go [Snapshot, (Threads numThreads)] = do
  currentCfg <- readConfig 
  newConfig <- aggregateEntriesDefaultExclude numThreads $ currentCfg { noEntries = True }
  writeConfig $ newConfig { noEntries = False }

go _ = putStrLn $ usageInfo "Usage: ljrss [OPTION]" options

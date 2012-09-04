module LJRSS.LJConfig where

import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Time (LocalTime)
import System.Directory (getAppUserDataDirectory, doesFileExist)
import Control.Monad (liftM)

type UserSessions = DM.Map String LocalTime

data TLJConfig = LJConfig {
  username, password :: String,
  notificationAddress :: String,
  notificationFromAddress :: String,
  ignored :: DS.Set String,
  networkTimeout :: Int,
  retryBeforeFail :: Int,
  sessions :: UserSessions,
  noEntries :: Bool
  } deriving (Read, Show)

appName = "ljrss"

getConfigFileName = getAppUserDataDirectory appName

readConfig :: IO TLJConfig
readConfig = do
  fileNameFromDir <- getConfigFileName
  go fileNameFromDir =<< doesFileExist fileNameFromDir
  where
    go fileNameFromDir haveFile = if not haveFile 
         then
            fail $ "Can not read config file " ++ fileNameFromDir
         else
            liftM read $ readFile fileNameFromDir

writeConfig :: TLJConfig -> IO ()
writeConfig cfg = do
  fileNameFromDir <- getConfigFileName
  writeFile fileNameFromDir $ show cfg

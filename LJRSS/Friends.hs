module LJRSS.Friends where

import Network.Curl
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as B8

newtype TLJFriend = LJFriend { getUsername :: String } deriving (Show)

-- http://www.livejournal.com/misc/fdata.bml?user=username

getLJFriends :: Curl -> String -> IO [TLJFriend]
getLJFriends curl username = withCurlDo $
  liftM (parseFriends . respBody) ( do_curl_ curl ljFriendsUrl [
      CurlTimeout 2,
      CurlUserAgent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0a2) Gecko/20110612 Firefox/6.0a2",
      CurlHttpHeaders [
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language: en-us,en;q=0.5",
        "Accept-Encoding: text/plain",
        "Connection: close"
      ]
    ] :: IO (CurlResponse_ [(String,String)] B8.ByteString))
  where
    ljFriendsUrl = "http://www.livejournal.com/misc/fdata.bml?user=" ++ username
    parseFriends = map (LJFriend . B8.unpack . B8.drop 2) . filter ( \f -> not (B8.null f) && B8.head f == '>') . B8.lines

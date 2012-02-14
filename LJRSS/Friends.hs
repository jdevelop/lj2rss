module LJRSS.Friends where

import Network.Curl
import Control.Monad (liftM)

newtype TLJFriend = LJFriend { getUsername :: String } deriving (Show)

getLJFriends :: String -> IO [TLJFriend]
getLJFriends username = withCurlDo $
  liftM (parseFriends . snd) $ curlGetString_ ljFriendsUrl [
      CurlTimeout 2,
      CurlUserAgent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0a2) Gecko/20110612 Firefox/6.0a2",
      CurlHttpHeaders [
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language: en-us,en;q=0.5",
        "Accept-Encoding: text/xml",
        "Connection: close"
      ]
    ]
  where
    ljFriendsUrl = "http://www.livejournal.com/misc/fdata.bml?user=" ++ username
    parseFriends = map (LJFriend . drop 2) . filter ( \f -> not (null f) && head f == '>') . lines

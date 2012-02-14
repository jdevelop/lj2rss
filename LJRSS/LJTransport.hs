module LJRSS.LJTransport where

import Debug.Trace
import Network.Curl
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Codec.Compression.GZip as ZL

getFeedContent :: String -> String -> String -> IO (Maybe String)
getFeedContent username password url = traceShow ( "begin: " ++ url) . withCurlDo $ do
  res <- curlGetString_ url [
      CurlHttpAuth [HttpAuthDigest],
      CurlTimeout 10,
      CurlUserAgent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0a2) Gecko/20110612 Firefox/6.0a2",
      CurlHttpHeaders [
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language: en-us,en;q=0.5",
        "Accept-Encoding: gzip",
        "Connection: close"
      ],
      CurlUserPwd (username ++ ":" ++ password)
    ]
  (return $!) $ processWithResponse res
  where
    processWithResponse (cCode, content) | cCode /= CurlOK = traceShow ( "Ooops" ++ (show cCode) ) Nothing
                                         | otherwise = traceShow ("end: " ++ url) $ Just . B8.toString . BS.concat . BL.toChunks $ ZL.decompress content

module LJRSS.LJTransport where

import Debug.Trace
import Network.Curl

getFeedContent :: String -> String -> String -> IO (Maybe String)
getFeedContent username password url = traceShow ( "begin: " ++ url) . withCurlDo $ do
  res <- curlGetString_ url [
      CurlHttpAuth [HttpAuthDigest],
      CurlTimeout 10,
      CurlUserAgent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0a2) Gecko/20110612 Firefox/6.0a2",
      CurlHttpHeaders [
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language: en-us,en;q=0.5",
        "Accept-Encoding: text/xml",
        "Connection: close"
      ],
      CurlUserPwd (username ++ ":" ++ password)
    ]
  (return $!) $ processWithResponse res
  where
    processWithResponse (cCode, content) | cCode /= CurlOK = traceShow ( "Ooops" ++ (show cCode) ) Nothing
                                         | otherwise = traceShow ("end: " ++ url) $ Just content

module LJRSS.LJTransport where

import Debug.Trace
import Network.Curl
import Control.Monad.Error

data TLJTransportError = LJTransportRetryable CurlCode | 
                         LJTransportFatal CurlCode | LJTransportNotice String

instance Error TLJTransportError where
  strMsg = LJTransportNotice

instance Show TLJTransportError where
  show (LJTransportNotice s) = s
  show (LJTransportFatal s) = "Fatal: " ++ (show s)
  show (LJTransportRetryable s) = "Retry: " ++ (show s)

type LJTransportMonad = ErrorT TLJTransportError IO  

getFeedContent :: String -> String -> String -> LJTransportMonad String
getFeedContent username password url = do
  (curlResponseCode, content) <- liftIO fetchContent
  case curlResponseCode of
    CurlOperationTimeout -> throwError $ LJTransportRetryable curlResponseCode
    CurlOK  -> (return $!) . traceShow ("end " ++ url) $ content
    otherwise -> throwError $ LJTransportFatal curlResponseCode
  where
    fetchContent = withCurlDo $ do
      traceShow ("Begin: " ++ url) $ curlGetString_ url [
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

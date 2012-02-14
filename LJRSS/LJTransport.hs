{-# LANGUAGE BangPatterns #-}
module LJRSS.LJTransport where

import Debug.Trace
import Network.Curl
import Control.Monad (liftM)
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Codec.Compression.GZip as ZL

getFeedContent :: Curl -> String -> String -> String -> IO (Maybe String)
getFeedContent curl username password url = withCurlDo $ do
  liftM processWithResponse (do_curl_ curl url [
      CurlHttpAuth [HttpAuthDigest],
      CurlTimeout 2,
      CurlUserAgent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0a2) Gecko/20110612 Firefox/6.0a2",
      CurlHttpHeaders [
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language: en-us,en;q=0.5",
        "Accept-Encoding: gzip",
        "Connection: close"
      ],
      CurlUserPwd (username ++ ":" ++ password)
    ] :: IO (CurlResponse_ [(String,String)] BL.ByteString))
  where
    processWithResponse !httpResp | respCurlCode httpResp /= CurlOK = Nothing
                                  | otherwise = traceShow url $ Just . B8.toString . BS.concat . BL.toChunks . ZL.decompress $ respBody httpResp

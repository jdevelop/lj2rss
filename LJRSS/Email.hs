module LJRSS.Email where

import Network.Mail.Mime

import qualified Data.Text.Lazy as DTL
import qualified Data.Text as DTS

import LJRSS.LJEntry

sendMail :: String ->
            String ->
            String ->
            TLJEntry ->
            IO ()
sendMail sender recipient journal (LJEntry link pubDate title text) = 
  simpleMail to from subject DTL.empty content [] >>= renderSendMail
  where
    addr src = Address Nothing $ DTS.pack src
    from = addr sender
    to = addr recipient
    content = DTL.pack text
    subject = DTS.pack $ "Update " ++ journal ++ " :: " ++ (show pubDate)

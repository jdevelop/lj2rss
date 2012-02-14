module LJRSS.TimeFormat where

import System.Locale
import Data.Time

-- Mon, 06 Feb 2012 19:06:59 GMT

parseLJTime :: String -> Maybe LocalTime
parseLJTime = parseTime defaultTimeLocale "%a, %d %b %Y %X %Z"

module Lib
    ( fetchEventPage 
    , parseEventTitles
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)
import Text.HTML.TagSoup (Tag(TagOpen, TagClose), innerText, partitions, (~==), (~/=))

fetchEventPage :: IO String
fetchEventPage = do
    req <- parseRequest "GET https://chessmaine.net/chessmaine/events/"
    resp <- httpLBS req
    return . L8.unpack $ getResponseBody resp

-- 1) `partitions` breaks the document into lists; here from starting h3 to just before the next one.
-- 2) in each of those lists, we don't care about tags after the subsequent closnig h3 tag.
-- 3) `innerText` joins all the text content in a list of tags (would catch e.g. the text in a span).
parseEventTitles :: [Tag String] -> [String]
parseEventTitles tags = innerText . takeWhile isNotCloseH3Tag <$> partitions isOpenEventTitleTag tags
  where
    isOpenEventTitleTag = (~== TagOpen ("h3" :: String) [("class", "entry-header")])
    isNotCloseH3Tag = (~/= TagClose ("h3" :: String))

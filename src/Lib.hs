module Lib
    ( fetchEventPage 
    , parseEventTitles
    , ScraperError(..)
    ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseStatusCode, getResponseBody)
import Text.HTML.TagSoup (Tag(TagOpen, TagClose), innerText, parseTags, partitions, (~==), (~/=))

data ScraperError
   = NetworkError Int
   | ParseError String
   deriving (Show)

fetchEventPage :: IO (Either ScraperError Text)
fetchEventPage = do
    req <- parseRequest "GET https://chessmaine.net/chessmaine/events/"
    resp <- httpLBS req
    let statusCode = getResponseStatusCode resp
    if statusCode == 200
      -- Decode the lazy ByteString to lazy Text, then convert to strict Text
      then return $ Right $ TL.toStrict $ TLE.decodeUtf8 $ getResponseBody resp
      else return $ Left (NetworkError statusCode)

-- 1) `partitions` breaks the document into lists; here from starting h3 to just before the next one.
-- 2) in each of those lists, we don't care about tags after the subsequent closing h3 tag.
-- 3) `innerText` joins all the text content in a list of tags (would catch e.g. the text in a span).
parseEventTitles :: Text -> [Text]
parseEventTitles html = innerText . takeWhile isNotCloseH3Tag <$> partitionedTags
  where
    partitionedTags = partitions isOpenEventTitleTag $ parseTags html
    isOpenEventTitleTag = (~== TagOpen ("h3" :: Text) [("class", "entry-header")])
    isNotCloseH3Tag = (~/= TagClose ("h3" :: Text))

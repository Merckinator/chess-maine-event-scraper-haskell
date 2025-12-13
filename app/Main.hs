module Main (main) where

import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.HTML.TagSoup (Tag(TagOpen), parseTags, innerText, (~/=))

fromEvents :: [Tag String] -> String
fromEvents = innerText . take 2 . dropWhile (~/= TagOpen ("h3" :: String) [("class","entry-header")])

main :: IO ()
main = do
    req <- parseRequest "GET https://chessmaine.net/chessmaine/events/"
    resp <- httpLBS req
    let body = L8.unpack $ getResponseBody resp
        tags = parseTags body
        eventTitle = fromEvents tags
    putStrLn eventTitle


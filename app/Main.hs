module Main (main) where

import Text.HTML.TagSoup (parseTags)
import Lib (fetchEventPage, parseEventTitles)

main :: IO ()
main = do
    -- TODO: make fetchEventPage return an Either; handle errors
    body <- fetchEventPage 
    let tags = parseTags body
        eventTitle = parseEventTitles tags
    -- TODO: do more useful things with the titles
    mapM_ putStrLn eventTitle


module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib (fetchEventPage, parseEventTitles, ScraperError(..))

handleError :: ScraperError -> IO ()
handleError (NetworkError code) = putStrLn $ "It seems the website is down. Status code: " ++ show code
handleError (ParseError msg)    = putStrLn $ "Could not parse the page: " ++ msg

handleSuccess :: [T.Text] -> IO ()
handleSuccess = TIO.putStrLn . T.unlines

main :: IO ()
main = do
    eitherResp <- fetchEventPage 
    let eitherTitles = parseEventTitles <$> eitherResp 
    either handleError handleSuccess eitherTitles
    -- TODO: do more useful things with the titles


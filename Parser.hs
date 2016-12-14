-- Parser.hs Writes Yahoo stock data from YQL into a JSON file

module Parser (writeStockData) where

import Network.HTTP (simpleHTTP,getRequest,getResponseBody)
import Network.HTTP.Client
import Network.HTTP.Headers
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.List
import Data.Char
import Text.Regex.Posix

writeStockData :: String -> String -> String -> IO ()
writeStockData company start end = do
    let url_temp = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.historicaldata%20where%20symbol\
    \%20%3D%20%22" ++ company ++ "%22%20and%20startDate%20%3D%20%22" ++ start ++ "%22%20and%20endDate%20%3D%20%22" ++ end ++ "%22&form\
    \at=json&diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="
    request <- parseRequest url_temp
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    let str_inp = parseResult . show . responseBody $ res
    writeFile "parsed_data.json" str_inp

parseResult :: String -> String
parseResult str = init . init . init . init . lowerStr $ cut str
    where
    cut :: String -> String
    cut [x] = [x]
    cut (x:xs)
        | isPrefixOf "quote\\\":" (x:xs) = drop 8 (x:xs)
        | otherwise = cut xs
    lowerStr :: String -> String
    lowerStr [] = []
    lowerStr [x] = [x]
    lowerStr (x:y:xs)
        | isAlpha x = toLower x : lowerStr (y:xs)
        | x == '\\' = lowerStr (y:xs)
        | x == '\"' && isDigit y = lowerStr (y:xs)
        | isDigit x && y == '\\' = x : lowerStr (drop 1 xs)
        | x == '-' = lowerStr (y:xs)
        | otherwise = x : lowerStr (y:xs)
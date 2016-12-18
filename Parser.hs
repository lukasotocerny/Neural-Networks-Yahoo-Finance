-- Parser.hs Writes Yahoo stock data from YQL into a JSON file

module Parser (writeStockData, getNDaysForw, getNDaysBack) where

import Network.HTTP (simpleHTTP,getRequest,getResponseBody)
import Network.HTTP.Client
import Network.HTTP.Headers
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.List
import Data.Char
import Text.Regex
import Text.Regex.Posix

generateUrl :: String -> String -> String -> String
generateUrl company start end = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.historicaldata%20where%20symbol\
    \%20%3D%20%22" ++ company ++ "%22%20and%20startDate%20%3D%20%22" ++ start ++ "%22%20and%20endDate%20%3D%20%22" ++ end ++ "%22&form\
    \at=json&diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="

generateUrlOpen :: String -> String -> String -> String
generateUrlOpen company start end = "https://query.yahooapis.com/v1/public/yql?q=select%20Open%20from%20yahoo.finance.historicaldata%20where%20symbol\
    \%20%3D%20%22" ++ company ++ "%22%20and%20startDate%20%3D%20%22" ++ start ++ "%22%20and%20endDate%20%3D%20%22" ++ end ++ "%22&form\
    \at=json&diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="

writeStockData :: String -> String -> String -> IO ()
writeStockData company start end = do
    let url_temp = generateUrl company start end
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
        | isPrefixOf "null" (x:xs) = error "Your request for financial data is not valid."
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

parseResultOpen :: String -> [Double]
parseResultOpen str = map read $ numbers $ cut str
    where
    cut :: String -> String
    cut [x] = [x]
    cut (x:xs)
        | isPrefixOf "null" (x:xs) = error "Your request for financial data is not valid."
        | isPrefixOf "quote\\\":" (x:xs) = (x:xs)
        | otherwise = cut xs
    numbers :: String -> [String]
    numbers xs = getAllTextMatches $ xs =~ "[0-9]+.[0-9]+" :: [String]

getNDaysForw :: String -> String -> Int -> IO [Double]
getNDaysForw company start n_forw = do
    let end = endDate start n_forw
    let url_temp = generateUrlOpen company start end
    request <- parseRequest url_temp
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    let open_prices = parseResultOpen . show . responseBody $ res
    return (reverse open_prices)

getNDaysBack :: String -> String -> Int -> IO [Double]
getNDaysBack company end n_back = do
    let start = startDate end n_back
    let url_temp = generateUrlOpen company start end
    request <- parseRequest url_temp
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    let open_prices = parseResultOpen . show . responseBody $ res
    return (reverse open_prices)

startDate :: String -> Int -> String
startDate end_date n = start_date end_int n
    where
    end_int :: [Int]
    end_int = map read $ splitRegex (mkRegex "-") end_date
    format :: Int -> String
    format n
        | 1 <= n && n <= 9 = '0' : show n
        | otherwise = show n
    start_date :: [Int] -> Int -> String
    start_date [yy,mm,dd] n
        | dd - n > 5 = format yy ++ "-" ++ format mm ++ "-" ++ format (dd-n-5)
        | mm - 1 > 0 = format yy ++ "-" ++ format (mm-1) ++ "-" ++ format (24-n)
        | otherwise = format (yy-1) ++ "-" ++  format 12 ++ "-" ++ format (24-n)

endDate :: String -> Int -> String
endDate start_date n = end_date start_int n
    where
    start_int :: [Int]
    start_int = map read $ splitRegex (mkRegex "-") start_date
    format :: Int -> String
    format n
        | 1 <= n && n <= 9 = '0' : show n
        | otherwise = show n
    end_date :: [Int] -> Int -> String
    end_date [yy,mm,dd] n
        | dd + n + 2 <= 28 = format yy ++ "-" ++ format mm ++ "-" ++ format (dd+n+2)
        | mm + 1 <= 12 = format yy ++ "-" ++ format (mm+1) ++ "-" ++ format (dd+n+2 `mod` 28)
        | otherwise = format (yy+1) ++ "-" ++  format 1 ++ "-" ++ format (dd+n+2 `mod` 28)
{-# LANGUAGE DeriveGeneric #-}
-- FrontEnd.hs Frontend for interaction with the backend neural network

import NeuralNet (construct, train, output, mistake, output_series, predict)
import Parser (writeStockData, getNDaysForw, getNDaysBack)
import Numeric.LinearAlgebra
import Data.Aeson
import Data.List
import GHC.Generics
import qualified Data.ByteString.Lazy as B

type TrainingSet = [([Double],[Double])]

data Quote = Quote {
    symbol :: String,
    date :: Int,
    open :: Double,
    high :: Double,
    low :: Double,
    close :: Double,
    volume :: Int,
    adj_close :: Double
    } deriving (Show, Generic)

instance FromJSON Quote
instance ToJSON Quote

-- TRAINING SECTION

-- Transforming the JSON data into a TrainingSet format with n-dimensional input vector
stockToData :: Int -> [Quote] -> TrainingSet
stockToData n (x:xs)
    | length (x:xs) >= n + 1 = [ (map ( \a -> open a ) $ take n (x:xs), [open $ head $ drop n (x:xs)] ) ] ++ stockToData n xs
    | otherwise = []

normalizeTrainData :: TrainingSet -> TrainingSet
normalizeTrainData [] = []
normalizeTrainData (x:xs) = (map norm (fst x), map norm (snd x)) : normalizeTrainData xs
    where
    max = maximum (fst x)
    min = minimum (fst x)
    norm = ( \a -> ( a-min ) / (2*( max-min )) )

-- PREDICTING SECTION

-- From an input saves the min, max values for a input vector
saveMinMax :: [[Double]] -> [(Double,Double)]
saveMinMax [] = []
saveMinMax (x:xs) = (min,max) : saveMinMax xs
    where
    max = maximum x
    min = minimum x

-- Normalizes the whole training set with min-max process, range [0,0.5]
normalize :: [Double] -> (Double,Double) -> [Double]
normalize xs (min,max) = map ( \x -> ( x-min ) / (2*( max-min )) ) xs

-- Finds the minmax values from the array of input vectors
minmax :: [[Double]] -> [Double] -> [(Double,Double)]
minmax inp init = scanl ( \(min,max) xs -> extract $ map (denorm min max) xs ) (minimum init, maximum init) inp
    where
    denorm = ( \a b x -> 2*(b-a)*x+a ) 
    extract = ( \x -> (minimum x, maximum x) )

-- With min, max as args it returns the denormalized out
denormalize :: [[Double]] -> [Double] -> [Double] -> [Double]
denormalize inp init out = zipWith ( \a (min,max) -> 2*(max-min)*a+min ) out (minmax inp init)

-- From an input of n days and following time series creates input vectors
inputs :: [Double] -> [Double] -> [[Double]]
inputs inp [] = []
inputs inp (o:os) = inp : inputs (tail inp ++ [o]) os

-- Stores raw data as time series in an array
writeRawData :: Maybe [Quote] -> String -> IO ()
writeRawData set path = writeFile path $ tail . init . show $ transform set
    where
    transform :: Maybe [Quote] -> [Double]
    transform Nothing = error "Error while loading data."
    transform (Just xs) = [ open x | x <- xs ]

-- MAIN

main :: IO ()
main = do 
    putStrLn "Write the stock market code of the company you want to look at (e.g. YHOO)"
    company <- getLine

    putStrLn "Write the start date of your training data (form YYYY-MM-DD)"
    start_date <- getLine

    putStrLn "Write the end date of your training data (form YYYY-MM-DD)"
    end_date <- getLine

    writeStockData company start_date end_date
    inp <- B.readFile "parsed_data.json"
    let raw_data = decode inp >>= ( \x -> Just (reverse x) ) :: Maybe [Quote]
    writeRawData raw_data "reality_out.csv"

    putStrLn "Write the number of days prior to our predicted day that shall be taken into account (e.g. 4)"
    day_dim <- readLn :: IO Int

    let inp_vectors_norm = raw_data >>= (\x -> Just (normalizeTrainData $ stockToData day_dim x)) :: Maybe TrainingSet
    let network = construct [day_dim,day_dim,1]

    putStrLn "Write the number of training iteration through the training set (e.g. 1000)"
    n_train <- readLn :: IO Int

    let network' = train network inp_vectors_norm 1.0 n_train

    putStrLn "Training successful."
    putStrLn ("Error: " ++ (show $ mistake network' inp_vectors_norm))
    putStrLn "Write the day from which you want to predict the stock (form YYYY-MM-DD)"
    test_start_date <- getLine :: IO String

    putStrLn "Write the number of days predicting (e.g. 10):"
    test_n_days <- readLn :: IO Int

    test_input <- getNDaysBack company test_start_date day_dim >>= ( \x ->  return (drop ((length x) - day_dim) x ))
    reality_check <- getNDaysForw company test_start_date test_n_days >>= ( \x ->  return (map ( \x -> (fromInteger $ round $ x * (10^3)) / (10.0^^3) ) $ take test_n_days x) )
    let test_input_norm = normalize test_input (minimum test_input, maximum test_input)
    let prediction = predict network' test_input_norm test_n_days
    let prediction_denorm = map ( \x -> (fromInteger $ round $ x * (10^3)) / (10.0^^3) ) $ denormalize (inputs test_input_norm prediction) test_input prediction
    
    putStrLn "Day X |  Network | Reality price ($)"
    putStrLn (foldl (++) [] [ "Day " ++ show day ++ " |  " ++ show net ++ "  | " ++ show real ++ "\n" | (net,real,day) <- zip3 prediction_denorm reality_check [1..] ] )
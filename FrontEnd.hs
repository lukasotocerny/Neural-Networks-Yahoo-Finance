{-# LANGUAGE DeriveGeneric #-}
-- FrontEnd.hs Frontend for interaction with the backend neural network

import NeuralNet (construct, train, output, mistake)
import Numeric.LinearAlgebra
import Parser
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

-- Transforming the JSON data into a TrainingSet format with n-dimensional input vector
stockToData :: Int -> [Quote] -> TrainingSet
stockToData n (x:xs)
    | length (x:xs) >= n + 1 = [ (map ( \a -> open a ) $ take n (x:xs), [open $ head $ drop n (x:xs)] ) ] ++ stockToData n xs
    | otherwise = []

-- Normalizes the whole training set with min-max process, range [0,1] (i.e. outputs as well)
normalize :: TrainingSet -> TrainingSet
normalize xs = map ( \x -> minmax x ) xs

-- The min-max normalizing function of input vectors
minmax :: ([Double],[Double]) -> ([Double],[Double])
minmax (xs,ys) = (map norm xs, map norm ys)
    where
    max = maximum (xs ++ ys)
    min = minimum (xs ++ ys)
    norm = ( \x -> ( x-min ) / ( max-min ) )

-- With min, max as args it returns the denormalized vector
denormalize :: [Double] -> Double -> Double -> [Double]
denormalize vec min max = map ( \x -> x * ( max-min ) + min ) vec

-- From an input saves the min, max values for a input vector
saveMinMax :: Maybe TrainingSet -> [(Double,Double)]
saveMinMax Nothing = []
saveMinMax (Just []) = []
saveMinMax (Just (x:xs)) = (min,max) : saveMinMax (Just xs)
    where
    max = maximum (fst x ++ snd x)
    min = minimum (fst x ++ snd x)

-- With a given test values it returns the output values for whole test set
writeNetworkOutput :: [[Double]] -> [(Double,Double)] -> String -> IO ()
writeNetworkOutput out con path = writeFile path $ tail . init . show . concat $ transform out con
    where
    transform :: [[Double]] -> [(Double,Double)] -> [[Double]]
    transform [] ys = []
    transform (x:xs) (y:ys) = denormalize x (fst y) (snd y) : transform xs ys

-- Stores raw data as time series in an array
writeRawData :: Maybe [Quote] -> String -> IO ()
writeRawData set path = writeFile path $ tail . init . show $ transform set
    where
    transform :: Maybe [Quote] -> [Double]
    transform Nothing = error "Error while loading data."
    transform (Just xs) = [ open x | x <- xs ]

main :: IO ()
main = do 
    writeStockData "YHOO" "2009-01-01" "2009-10-01"
    inp <- B.readFile "parsed_data.json"
    let raw_data = decode inp :: Maybe [Quote]
    writeRawData raw_data "reality_out.csv"
    let inp_vectors = raw_data >>= (\x -> Just (stockToData 6 x)) :: Maybe TrainingSet
    let coefficients = saveMinMax inp_vectors
    let inp_vectors_norm = inp_vectors >>= (\x -> Just (normalize x)) :: Maybe TrainingSet
    let network = construct [6,6,1]
    let network' = train network inp_vectors_norm 1.0 100
    putStrLn (show network')
    let network_out = output network' inp_vectors_norm
    putStrLn (show network_out)
    let series = zipWith (\a (b,c) -> denormalize a b c) (output network' inp_vectors_norm) coefficients
    writeNetworkOutput network_out coefficients "network_out.csv"
    putStrLn (show $ mistake network' inp_vectors_norm)
    putStrLn ("Success.")
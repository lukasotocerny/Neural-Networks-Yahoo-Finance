-- Neural nets output

import Numeric.LinearAlgebra

type Network = [Matrix Double]

-- Builds networks with initial weights=1 and dimensions: input, hidden 1, hidden 2.., output layer
buildNetwork :: [Int] -> Network
buildNetwork xs = [ (x><y) (repeat 1) | (x, y) <- zip xs (tail xs) ]

-- Takes [Double] Input and folds across layer weight matrices
propagate :: Network -> [Double] -> [Double]
propagate ns xs = head $ toLists (output ns input)
    where 
    n = length xs
    input = (1><n) xs :: Matrix Double

-- Folding with matrix multiplication across weight matrices
output :: Network -> Matrix Double -> Matrix Double
output ms xs = foldl (<>) xs ms

-- For future network training
calcErr :: [Double] -> [Double] -> Double
calcErr ys ts = 0.5 * ( sum $ zipWith (\x y -> (x - y)^2) ys ts )

main = do 
    putStrLn ("Write the number of input, hidden and output neurons respectively (e.g. 2 5 1):")
    dim_temp <- getLine
    let dim = map read (words dim_temp)
    let network = buildGenNetwork dim
    putStrLn ("Write the input:")
    dim_inp <- getLine
    let inp = map read (words dim_inp)
    putStrLn (show $ propagate network inp)
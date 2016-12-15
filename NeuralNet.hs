-- NeuralNet.hs Backend for the neural net training

module NeuralNet (construct, train, output, mistake, output_series, predict) where

import Numeric.LinearAlgebra
import Data.List
import System.Random

-- Data types with constructors
data LayerWeights = LayerWeights { weights :: Matrix Double }
    deriving (Show)
data LayerOutput = LayerOutput { weightsIn :: Matrix Double, outputs :: Matrix Double, inputs :: Matrix Double }
                 | InputLayer { outputs :: Matrix Double, inputs :: Matrix Double }
    deriving (Show)
data LayerError = LayerError { errors :: Matrix Double }
    deriving (Show)

type Network = [ LayerWeights ]
type PassedNetwork = [ LayerOutput ]
type TrainingSet = [([Double],[Double])]

-- Builds networks with random initial weights and dimensions: input, hidden 1, hidden 2.., output layer
construct :: [Int] -> Network
construct xs = [ LayerWeights { weights = (x><y) (randoms $ mkStdGen 1) } | (x, y) <- zip xs (tail xs) ]

-- Sigmoid activation function
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp(-x)) 

-- Matrix multiplication with incorporated sigmoid operation
matrixMult :: Matrix Double -> Matrix Double -> Matrix Double
matrixMult xs ys = fromLists [ [ sigmoid ( sum (zipWith (*) x y) ) | y <- toLists (tr' ys) ] | x <- toLists xs ]

-- Propagates output from layer i to layer j through weights, saves output and input
propagate :: LayerOutput -> LayerWeights -> LayerOutput
propagate layerI layerJ = LayerOutput { weightsIn = weiJ, outputs = (matrixMult outI weiJ), inputs = outI }
    where
    outI = outputs layerI :: Matrix Double
    weiJ = weights layerJ :: Matrix Double

-- Forward pass of input through scanl folding function (output is propagated as row vector)
forwardPass :: Network -> [Double] -> PassedNetwork
forwardPass net inp = scanl propagate layer0 net
    where
    inp_dim = length inp
    layer0 = InputLayer { outputs = (1><inp_dim) inp, inputs = (1><inp_dim) inp }

-- Takes error from layer j and output from layer i and calculated the error for layer i
backpropagate :: LayerOutput -> LayerError -> LayerError
backpropagate layerI layerJ = LayerError { errors = sigmoid_der * sumOfErrs }
    where
    weightsIJ = weightsIn layerI :: Matrix Double 
    outI = tr' (inputs layerI) :: Matrix Double
    dim_outI = rows weightsIJ :: Int
    ones = (dim_outI><1) (repeat 1.0) :: Matrix Double
    sigmoid_der = outI * ( ones + ((-1)*outI) )
    err = errors layerJ
    sumOfErrs = weightsIJ <> err

-- Backward pass of errors through scanr function (error is propagated as column vector)
backwardPass :: PassedNetwork -> [Double] -> [LayerError]
backwardPass net target = scanr backpropagate layerN (drop 2 net)
    where
    output = toList ( flatten $ outputs (last net) )
    dim_out = length output :: Int
    ones = (dim_out><1) (repeat 1.0) :: Matrix Double
    diff = (dim_out><1) (zipWith (-) target output) :: Matrix Double
    out = (dim_out><1) output
    layerN = LayerError { errors = out * ( ones + ((-1)*out) ) * diff :: Matrix Double }

-- The zipping function for updating weights.
updateWeights :: Double -> LayerOutput -> LayerError -> LayerWeights
updateWeights eps (LayerOutput {weightsIn=x,outputs=out,inputs=z}) (LayerError {errors=err}) = LayerWeights {weights=x+rate*matrix}
    where
    matrix = err <> out
    dim = size matrix
    rate = (fst dim><snd dim) (repeat eps)

-- Completes a whole iteration, return new set of weights. Args: network, input, target, learning rate
update :: Network -> [Double] -> [Double] -> Double -> Network
update net input target rate = zipWith (updateWeights rate) (tail passedNet) errors
    where
    passedNet = forwardPass net input
    errors = backwardPass passedNet target

-- Returns the denormalized output for a give single input vector
output :: Network -> [Double] -> Double
output net inp = head . head . toLists $ outputs (last $ forwardPass net inp)

-- Returns the sum of mistakes on the whole data set
mistake :: Network -> Maybe TrainingSet -> Double
mistake net Nothing = error "Request for financial data is incorrect."
mistake net (Just []) = 0
mistake net (Just (x:xs)) = errs + mistake net (Just xs)
    where
    outs = head . toLists . outputs . last $ forwardPass net (fst x)
    errs = sum $ zipWith ( \x y -> 0.5*(x-y)*(x-y) ) (snd x) outs

-- Iterates the network on the same training set n times
train :: Network -> Maybe TrainingSet -> Double -> Int -> Network
train net Nothing rate i = error "Request for financial data is incorrect."
train net (Just set) rate 0 = net
train net (Just set) rate i = train (foldr (\a b -> update b (fst a) (snd a) rate) net set) (Just set) rate (i-1)

-- Network prediction for n days, taking k-dimensional input vector
predict :: Network -> [Double] -> Int -> [Double]
predict net inp 0 = []
predict net inp days = prediction : predict net new_inp (days-1)
    where
    prediction = output net inp
    new_inp = tail inp ++ [prediction]
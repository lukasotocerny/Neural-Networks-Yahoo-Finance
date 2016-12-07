-- Neural net backpropagation

module NeuralNet (LayerWeights, buildNetwork, iteration) where

import Numeric.LinearAlgebra
import Data.List

-- Data types with constructors
data LayerWeights = LayerWeights { weights :: Matrix Double }
data LayerOutput = LayerOutput { outWeights :: Matrix Double, outputs :: Matrix Double, inputs :: Matrix Double } | InputLayer { outputs :: Matrix Double, inputs :: Matrix Double }
    deriving (Show)
data LayerError = LayerError { errors :: Matrix Double }
    deriving (Show)

type Network = [ LayerWeights ]
type PassedNetwork = [ LayerOutput ]
type TrainingSet = [([Double],[Double])]

-- Builds networks with initial weights=1 and dimensions: input, hidden 1, hidden 2.., output layer
buildNetwork :: [Int] -> Network
buildNetwork xs = [ LayerWeights { weights = (x><y) (repeat 1) } | (x, y) <- zip xs (tail xs) ]

-- Sigmoid activation function
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp(-x)) 

-- Matrix multiplication with incorporated sigmoid operation
matrixMult :: Matrix Double -> Matrix Double -> Matrix Double
matrixMult xs ys = fromLists [ [ sigmoid ( sum (zipWith (*) x y) ) | y <- toLists (tr' ys) ] | x <- toLists xs ]

-- Propagates output from layer i to layer j through weights, saves output and input
propagate :: LayerOutput -> LayerWeights -> LayerOutput
propagate layerI layerJ = LayerOutput { outWeights = weiJ, outputs = (matrixMult outI weiJ), inputs = outI }
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
    weights = outWeights layerI :: Matrix Double 
    out = tr' (inputs layerI) :: Matrix Double
    dim_out = rows weights :: Int
    ones = (dim_out><1) (repeat 1.0) :: Matrix Double
    sigmoid_der = out * ( ones + ((-1)*out) )
    err = errors layerJ
    sumOfErrs = weights <> err

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
updateWeights eps (LayerOutput {outWeights=x,outputs=out,inputs=z}) (LayerError {errors=err}) = LayerWeights {weights=out+rate*matrix}
    where
    matrix = out <> err
    dim = size matrix
    rate = (fst dim><snd dim) (repeat eps)

-- Completes a whole iteration, return new set of weights. Args: network, input, target, learning rate
iteration :: Network -> [Double] -> [Double] -> Double -> Network
iteration net input target rate = zipWith (updateWeights rate) (tail passedNet) errors
    where
    passedNet = forwardPass net input
    errors = backwardPass passedNet target

-- Format training data set
input :: TrainingSet -> [[Double]]
input xs = [ fst x | x <- xs ]
target :: TrainingSet -> [[Double]]
target xs = [ snd x | x <- xs ]
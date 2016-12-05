-- Neural net backpropagation
-- Training not implemented yet. Forward and backward passes work

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
backwardPass :: PassedNetwork -> [Double] -> [Double] -> [LayerError]
backwardPass net output target = scanr backpropagate layerN (drop 2 net)
    where
    dim_out = length output :: Int
    ones = (dim_out><1) (repeat 1.0) :: Matrix Double
    diff = (dim_out><1) (zipWith (-) target output) :: Matrix Double
    out = (dim_out><1) output
    layerN = LayerError { errors = out * ( ones + ((-1)*out) ) * diff :: Matrix Double }

-- Testing network
testNet :: Network
testNet = [ LayerWeights { weights = (2><2) [0.1,0.4,0.8,0.6] }, LayerWeights { weights = (2><1) [0.3,0.9] } ]
passedNet = forwardPass testNet [0.35,0.9]

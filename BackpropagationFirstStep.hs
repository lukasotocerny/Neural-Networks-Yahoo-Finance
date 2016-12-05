-- Neural net backpropagation
-- Contains bugs, does not compile

import Numeric.LinearAlgebra
import Data.List

data LayerWeights = LayerWeights { weights :: Matrix Double }
data LayerOutput = LayerOutput { outWeights :: Matrix Double, outputs :: Matrix Double } | InputLayer { outputs :: Matrix Double }
    deriving (Show)
data LayerError = LayerError { errors :: Matrix Double }
    deriving (Show)

type Network = [ LayerWeights ]
type PassedNetwork = [ LayerOutput ]

-- Builds networks with initial weights=1 and dimensions: input, hidden 1, hidden 2.., output layer
buildNetwork :: [Int] -> Network
buildNetwork xs = [ LayerWeights { weights = (x><y) (repeat 1) } | (x, y) <- zip xs (tail xs) ]

-- Takes LayerOutput and next layer weights and produces LayerOutput
propagate :: LayerOutput -> LayerWeights -> LayerOutput
propagate layerI layerJ = LayerOutput { outWeights = weiJ, outputs = (outI <> weiJ) }
    where
    outI = outputs layerI :: Matrix Double
    weiJ = weights layerJ :: Matrix Double

forwardPass :: Network -> [Double] -> PassedNetwork
forwardPass net inp = scanl propagate layer0 net
    where
    inp_dim = length inp
    layer0 = InputLayer { outputs = (1><inp_dim) inp }

backpropagate :: LayerOutput -> LayerError -> LayerError
backpropagate layerI layerJ = LayerError { errors = sigmoid_der * sumOfErrs }
    where
    weights = outWeights layerI :: Matrix Double 
    out = outputs layerI :: Matrix Double
    dim_out = rows out :: Int
    ones = (dim_out><1) (repeat 1.0) :: Matrix Double
    sigmoid_der = out * ( ones + ((-1)*out) )
    err = errors layerJ
    sumOfErrs = weights <> err

backwardPass :: PassedNetwork -> [Double] -> [Double] -> [LayerError]
backwardPass net output target = scanr backpropagate layerN (tail net)
    where
    dim_out = length output :: Int
    ones = (dim_out><1) (repeat 1.0) :: Matrix Double
    diff = (dim_out><1) (zipWith (-) target output) :: Matrix Double
    out = (dim_out><1) output
    layerN = LayerError { errors = out * ( ones + ((-1)*out) ) * diff :: Matrix Double }
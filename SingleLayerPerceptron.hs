-- Single Layer Perceptrons
import System.Random

type Weight = Double
type Bias = Double

-- Class with [Weights] Bias
data Perceptron = Neuron [Weight] Bias
    deriving ( Eq, Show )

-- Evaluating the output of Perceptron given the Input vector
evaluate :: Perceptron -> [Double] -> Int
evaluate (Neuron w b) xs
    | sum (zipWith (*) w xs) >= -b = 1
    | otherwise = 0

-- Updating the Perceptron given the Input vector and Target value
update :: Perceptron -> [Double] -> Int -> Perceptron
update (Neuron w b) xs t = Neuron (zipWith (+) w (map (* floatDif) xs)) (b + floatDif)
    where
    y = evaluate (Neuron w b) xs
    floatDif = fromIntegral (t - y)

-- Learning the Perceptron on a set of training data of input vectors and target values
train :: Perceptron -> [([Double],Int)] -> Perceptron
train neuron [] = neuron
train neuron (x:xs) = train (update neuron (fst x) (snd x)) xs

-- Evaluating whether the Perceptron output the target values
success :: Perceptron -> [([Double],Int)] -> Bool
success neuron [] = True
success neuron (x:xs) = evaluate neuron (fst x) == (snd x) && success neuron xs

-- Iterates the training n times. If successful, it breaks.
iterate_train :: Perceptron -> [([Double],Int)] -> Int -> Perceptron
iterate_train neuron xs 0 = error "Cannot train Perceptron. Data are hard to clasify."
iterate_train neuron xs i
    | success neuron xs = neuron
    | otherwise = iterate_train (train neuron xs) xs (i - 1)

buildNeuron :: Int -> Perceptron
buildNeuron x = Neuron [ randomRIO (0.0,1.0) | i <- [1..x] ] (round randomRIO (0.0,1.0) getStdGen) 

main = do 
    putStrLn ("Write the number of input neurons:")
    nInput <- readLn :: IO Int
    putStrLn (show nInput)
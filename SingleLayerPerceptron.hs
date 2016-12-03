-- Single Layer Perceptrons
import Data.List
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
iterate_train neuron xs 0 = neuron
iterate_train neuron xs i
    | otherwise = iterate_train (train neuron xs) xs (i - 1)

buildNeuron :: Int -> Perceptron
buildNeuron x = Neuron [ 1.0 | i <- [1..x] ] (1.0) 

getTLines :: Int -> IO [String]
getTLines 0 = return []
getTLines t = do
    x <- getLine
    xs <- getTLines (t-1)
    return (x:xs)

createData :: [String] -> [([Double],Int)]
createData xs = [ (init test, round (last test)) | test <- doubles ]
    where
    doubles = [ map read (words x) | x <- xs ] :: [[Double]]

main = do 
    putStrLn ("Write the number of input neurons:")
    n_temp <- getLine
    let n = read n_temp :: Int
    putStrLn ("Write the number of test cases:")
    t_temp <- getLine
    let t = read t_temp :: Int
    putStrLn ("Write the test cases in form: i1 i2 .. in t")
    putStrLn ("Where t is target output, i are inputs.")
    putStrLn ("For example: 1 4 1 1 0")
    d_temp <- getTLines t
    putStrLn ("How many times do you want to iterate training?")
    i_temp <- getLine
    let i = read i_temp :: Int
    let dataset = createData d_temp
    let neuron = buildNeuron n
    let neuron' = iterate_train neuron dataset i
    if success neuron' dataset then do
        putStrLn ("Successfully trained. Resulting weights are:")
        putStrLn (show neuron')
    else do
        putStrLn ("Neuron not trained on the dataset. Weights after training are:")
        putStrLn (show neuron')
<h2>Haskell implementation of Neural Networks</h2>
<h3>Introduction</h3>
This project aims to analyze the US Stock Market through a simple single-hidden layer neural network, using the backpropagation algorithm.
The data are fetched from Yahoo API, using YQL queries. There are 3 modules in this repository, each is described bellow. 

<h3>FrontEnd.hs</h3>
The `FrontEnd.hs` file is used for user interaction and serves as the combining element of `Parser` and `NeuralNet` module. It has couple of methods.


```haskell
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
```

This is a data constructor corresponding to the information received in JSON format from YQL API. Module `Data.Aeson` converts JSON data into 
this data type generically through the `decode` command.


```haskell
stockToData :: Int -> [Quote] -> [([Double],[Double])]
```

This method generates n-dimensional inputs with the target value, in the form (input vector, target). Array is used in case of multidimensional target.

<h3>NeuralNet.hs</h3>
serves a front-end for the back-end training in `NeuralNet` module and parsing of data into JSON files from the `Parser` module. So far the results 
look reasonable, I shall do statistical analysis in the future. `reality_out.csv` is a comma-separated-value file of the real quotes, whereas 
`network_out.csv` shows the output of the network. `parsed_data.json` is a JSON file containg the raw (partially parsed) input from the YQL query.
I still have to separate the training set and test set, currently training and test sets are one and other.


Of course, having n-dimensional input vector for n days opening price of a share, it is not that hard to output the value for (n+1)-opening day. 
Easy naive algorithm would be to average all of the days, and the result would not be that far off. But what is network unique in, compared to the 
simple algorithm is it's general, can be used for different input information. Once it's trained, it only propagates inputs to generate output. 
I shall try using other financial indicators and see the results underneath.

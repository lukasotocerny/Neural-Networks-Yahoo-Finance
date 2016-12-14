<h2>Haskell implementation of Neural Networks</h2>
<h3>Introduction</h3>
Currently taking the University of Toronto's Neural Networks Coursera course, I was keen to try to take my first steps into the world
of machine learning and artificial intelligence. So far, I have created only two models. First one is fully functional classification 
perceptron algorithm in `SingleLayerPerceptron.hs` file, which is cute, but has several limitations. It takes a n-dimensional input vector and return binary classification.
It can be trained only on linearly seperable dataset.


The other files are project of the analysis of Stock Market, using data from the YQL queries in Yahoo Finance. The `FrontEnd.hs` file 
serves a front-end for the back-end training in `NeuralNet` module and parsing of data into JSON files from the `Parser` module. So far the results 
look reasonable, I shall do statistical analysis in the future. `reality_out.csv` is a comma-separated-value file of the real quotes, whereas 
`network_out.csv` shows the output of the network. `parsed_data.json` is a JSON file containg the raw (partially parsed) input from the YQL query.
I still have to separate the training set and test set, currently training and test sets are one and other.


Of course, having n-dimensional input vector for n days opening price of a share, it is not that hard to output the value for (n+1)-opening day. 
Easy naive algorithm would be to average all of the days, and the result would not be that far off. But what is network unique in, compared to the 
simple algorithm is it's general, can be used for different input information. Once it's trained, it only propagates inputs to generate output. 
I shall try using other financial indicators and see the results underneath.

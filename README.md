<h2>Haskell implementation of Neural Networks</h2>
<h3>Introduction</h3>
<p>Currently taking the University of Toronto's Neural Networks Coursera course, I was keen to try to take my first steps into the world
of machine learning and artificial intelligence. So far, I have created only two models. First one is fully functional classification 
perceptron algorithm, which is cute, but has several limitations. It takes a n-dimensional input vector and return binary classification.
It can be trained only on linearly seperable dataset.</p>
The second model is a more complex one. It is a neural net. Running the `main` function shall request the dimension of input, hidden and
output layers, initializing all the weights to be 1. Afterwards it takes an input vector and propagates it through the net and returns 
output. It is a first working stage for the development of backpropagation algorithm.

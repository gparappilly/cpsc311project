# Using Neural Networks for Document Classification

We have implemented an artificial neural network in Erlang, trained it, and then applied it to the document classification problem, that is the problem of determining its author upon input of a text passage. The end product allows users to, given a text document, determine if the style of the document matches any of the authors included in the dataset that the algorithm has been trained on. Given sufficient examples of an author’s work, it should also be possible to train the system to recognize new authors who have styles that are sufficiently distinct from those that have already been input. Since training a neural network often requires processing large data sets, we have strived for a concurrent implementation which makes use of all the available processing power. The value of this project is manifold. It demonstrates, through the application to machine learning, which has gained much popularity recently, that concurrency is a topic of extreme relevance these days. It shows that Erlang features good built-in support for concurrent programming, making it a potentially useful language. The project also allows us to display our ability to rapidly learn an unknown programming language and exploit its benefits to write a substantial program. 

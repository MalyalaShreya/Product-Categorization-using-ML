# Product Categorization using Supervised Machine Learning

### Objectives 
* Given a product, and it's details, classified it into 3 broad categories namely,
	* Automobiles
	* Electronics
	* Home

* Compare different classifiers, namely Naive Bayes, Kmeans and Neural Networks

### Procedure followed
* Parse the data-sets to get the tokens
* Get the keywords of the data-set and sort them according to frequency
* Parse the Train and Test sets and tokenize the data
* Convert the data into numeric data-set and store in database
* Use the classifiers to find the category of the product

### Steps to run the program

> $ cabal install hnn

> $ cabal install stemmer

> $ ghc Main.hs

> $ ghc ./Main

### Results

```
Filter for Neural Networks
Number of Layers : 3
Number of neurons : 9 [9] 3
Number of times trained : 1000
Threshold value : 0.7
```

* Test Case 1

| Actual Product | Neural Networks | Kmeans | Naive Bayes |
|:---:|:---:|:---:|:---:|
| Automobiles | Automobiles | Automobiles | Automobiles |
| Automobiles  | Electronics | Automobiles | Automobiles |
| Electronics  | Electronics | Electronics | Electronics |
| Electronics  | Automobiles | Electronics | Electronics |
| Home  | Automobiles | Electronics | Home |
| Home | Automobiles | Electronics | Home |

&nbsp;

* Test Case 2

| Actual Product | Neural Networks | Kmeans | Naive Bayes |
|:---:|:---:|:---:|:---:|
| Automobiles | Automobiles | Home | Automobiles |
| Automobiles  | Electronics | Automobiles | Automobiles |
| Electronics  | Electronics | Electronics | Electronics |
| Electronics  | Electronics | Electronics | Electronics |
| Home | Electronics | Electronics | Home |
| Home | Home | Home | Automobiles |

&nbsp;

* Test Case 3

| Actual Product | Neural Networks | Kmeans | Naive Bayes |
|:---:|:---:|:---:|:---:|
| Automobiles | Home | Home | Automobiles |
| Automobiles  | Home | Automobiles | Automobiles |
| Electronics  | Automobiles | Automobiles | Electronics |
| Electronics  | Automobiles | Electronics | Electronics |
| Home  | Automobiles | Home | Home |
| Home | Automobiles | Home | Home|

&nbsp;

* Test Case 4

| Actual Product | Neural Networks | Kmeans | Naive Bayes |
|:---:|:---:|:---:|:---:|
| Automobiles | Home | Automobiles | Automobiles |
| Automobiles  | Home | Automobiles | Automobiles |
| Electronics  | Electronics | Automobiles | Electronics |
| Electronics  | Electronics | Electronics | Electronics |
| Home  | Automobiles | Automobiles | Home |
| Home | Automobiles | Home | Home |

&nbsp;

* Test Case 5

| Actual Product | Neural Networks | Kmeans | Naive Bayes |
|:---:|:---:|:---:|:---:|
| Automobiles | Automobiles | Automobiles | Automobiles |
| Automobiles  | Home | Automobiles | Automobiles |
| Electronics  | Electronics | Electronics | Electronics |
| Electronics  | Automobiles | Electronics | Electronics |
| Home  | Automobiles | Home | Home |
| Home | Automobiles | Home | Home |

&nbsp;

### Observartions
* The correctness of Neural Networks depends on the following:
	* Number of layers
	* Number of neurons in each layer
	* Number of times the data is trained
	* Threshold value
	* Activation function
	* Random values generated 
* The correctness of Naive Bayes depends on the following:
	* Variance and mean of training set

* The correctness of Kmeans depends on the following:
	* Training set
	* k nearest neighbours



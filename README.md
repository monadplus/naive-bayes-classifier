## naive-bayes-classifier

Joint probability is untractable.

Bayes Theorem:

```
P(A|B) = P(B|A) * P(A) / P(B)

P(A|B) = "posterior"
P(A) = "prior"
```

Classification as:

```
P(yi | x1, x2, …, xn) = P(x1, x2, …, xn | yi) * P(yi) / P(x1, x2, …, xn)
```

`P(yi)` can be easy computed but `P(x1, x2, …, xn | yi)` is not feasible.

To simplify, we are going to consider each variable independent from each other.

First, the denominator is removed from the calculation P(x1, x2, …, xn) as it is a constant used in calculating the conditional probability of each class for a given instance and has the effect of normalizing the result.

```
P(yi | x1, x2, …, xn) = P(x1, x2, …, xn | yi) * P(yi)
```

Next, the conditional probability of all variables given the class label is changed into separate conditional probabilities of each variable value given the class label. These independent conditional variables are then multiplied together. For example:

```
P(yi | x1, x2, …, xn) = P(x1|yi) * P(x2|yi) * … P(xn|yi) * P(yi)
```

The simplification of Bayes Theorem assuming independency of the variables is called __Naive Bayes__ and it is widely used as a classification predictive modeling.

The Naive Bayes algorithm has proven effective and therefore is popular for __text classification tasks__. The words in a document may be encoded as binary (word present), count (word occurrence), or frequency (tf/idf) input vectors and binary, multinomial, or Gaussian probability distributions used respectively.

### Data Sets

- Wine Quality: https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009#winequality-red.csv
- Titanic: unknown

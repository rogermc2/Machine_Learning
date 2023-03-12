import numpy as np

from sklearn.neural_network import MLPClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import MultinomialNB

import matplotlib
from matplotlib import pyplot as plt

def init_mlp():
    return MLPClassifier(hidden_layer_sizes=[], activation='identity', max_iter = 10000)

def init_logistic_regression():
    return LogisticRegression(max_iter = 10000)

def init_MultinomialNB():
    return MultinomialNB()

def fit (est, X, y):
    est.fit(X, np.ravel(y))

def predict (est, X):
    return tuple(est.predict(X))

def score (est, X, y):
    return est.score(X, np.ravel(y))

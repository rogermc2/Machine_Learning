import numpy as np

from sklearn.neural_network import MLPClassifier
from sklearn.linear_model import LogisticRegression

import matplotlib
from matplotlib import pyplot as plt

def init_mlp(nhidden):
    return MLPClassifier(hidden_layer_sizes=nhidden, max_iter = 50000)

def init_logistic_regression():
    return LogisticRegression(max_iter = 500)

def fit (est, X, y):
    est.fit(X, y)

def predict (est, X):
    return tuple(est.predict(X))

def predict_proba (est, X):
    return tuple(est.predict_proba(X))

def plot(degrees, trainerr, testerr):
    plt.scatter(degrees, trainerr)
    plt.plot(degrees, trainerr, label='training error')
    plt.scatter(degrees, testerr)
    plt.plot(degrees, testerr, label='test error')
    plt.legend()
    plt.title('Prediction error by polynomial kernel degree')
    plt.xlabel('degree')
    plt.ylabel('accuracy')
    plt.show()

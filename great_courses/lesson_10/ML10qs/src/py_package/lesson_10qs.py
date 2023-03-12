import numpy as np

from sklearn.neural_network import MLPClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import MultinomialNB

import matplotlib
from matplotlib import pyplot as plt

def init_mlp():
    return MLPClassifier(hidden_layer_sizes=[], activation='identity', max_iter = 10000)

def init_logistic_regression():
    return LogisticRegression(max_iter = 500)

def init_MultinomialNB():
    return MultinomialNB()

def fit (est, X, y):
    est.fit(X, np.ravel(y))

def predict (est, X):
    return tuple(est.predict(X))

def predict_proba (est, X):
    X_array = np.asarray(X).reshape(1, -1)
    return tuple(map(tuple, est.predict_proba(X_array)))

def plot(clf):
    feats = ['Pclass','Sex','Age','SibSp','Parch','Fare', 'Embarked S', 'Embarked C', 'Embarked Q']
    print(feats)
    for i in range(len(feats)):
        print(feats[i], clf.coef_[0][i])


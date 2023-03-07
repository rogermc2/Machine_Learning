import numpy as np

from sklearn import tree

import matplotlib
from matplotlib import pyplot as plt

def init_tree(nodes):
    return tree.DecisionTreeClassifier(max_leaf_nodes=nodes)

def fit (est, X, y):
    est.fit(X, np.ravel(y))

def predict (est, X):
    return tuple(est.predict(X))

def plot(ms, minierr, testerr):
    plt.scatter(ms, minierr)
    plt.plot(ms, minierr, label = 'training error')
    plt.scatter(ms, testerr)
    plt.plot(ms, testerr, label = 'test error')
    plt.legend()
    plt.title('Prediction error by training data size')
    plt.xlabel('train size')
    plt.ylabel('error')
    plt.show()

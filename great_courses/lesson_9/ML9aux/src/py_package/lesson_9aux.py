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

def plot(leaves, trainerr, testerr):
    plt.scatter(leaves, trainerr)
    plt.plot(leaves, testerr)
    plt.title('Prediction error by number of leaves')
    plt.xlabel('leaves')
    plt.ylabel('error')
    plt.show()

import numpy as np

from sklearn import neighbors

import matplotlib
from matplotlib import pyplot

def init_NeighborsClassifier(k):
    return neighbors.KNeighborsClassifier(n_neighbors=k,metric="cosine")

def fit (est, X_train, y_train):
    est.fit(X_train, y_train)

def predict (est, X):
    return tuple(est.predict(X))

def plot_prediction(X, y, X_lots, y_gp):
    pyplot.scatter(X, y)
    pyplot.plot(X_lots, y_gp)
    pyplot.title('Function')
    pyplot.xlabel('Y')
    pyplot.ylabel('X')
    pyplot.show()

def print_program (est_gp):
    print ("genetic program:")
    print("  ", est_gp._program)

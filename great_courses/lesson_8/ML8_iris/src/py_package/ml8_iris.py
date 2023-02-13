import numpy as np

from sklearn import neighbors

import matplotlib
from matplotlib import pyplot

def init_NeighborsClassifier(k):
    return neighbors.KNeighborsClassifier(algorithm='auto', leaf_size=30, metric='minkowski', metric_params=None, n_jobs=1, n_neighbors=k, p=2, weights='uniform')

def fit (est, X, y):
    est.fit(X, y)

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

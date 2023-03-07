import numpy as np

from sklearn import neighbors
from sklearn import svm

def init_neighbours():
    return neighbors.KNeighborsClassifier(n_neighbors=1,metric="cosine")

def init_svc(k):
    return svm.SVC(kernel=k)

def fit (est, X, y):
    est.fit(X, y)

def predict (est, X):
    return tuple(est.predict(X))

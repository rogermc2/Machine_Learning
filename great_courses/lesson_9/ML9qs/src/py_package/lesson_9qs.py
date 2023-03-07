import numpy as np

from sklearn import neighbors
from sklearn import svm

import matplotlib
from matplotlib import pyplot as plt

def init_neighbours():
    return neighbors.KNeighborsClassifier(n_neighbors=1,metric="cosine")

def init_svc(k):
    return svm.SVC(kernel=k)

def fit (est, X, y):
    est.fit(X, y)

def predict (est, X):
    return tuple(est.predict(X))

def plot(acc):
    plt.plot([int(v[0]) for v in acc], [v[1] for v in acc], c="blue")
    plt.plot([int(v[0]) for v in acc], [v[2] for v in acc], c="red")
    plt.title('malware')
    plt.xlabel('k')
    plt.ylabel('acc')
    plt.show()

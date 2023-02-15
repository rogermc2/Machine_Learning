import numpy as np

from sklearn import neighbors

import matplotlib
from matplotlib import pyplot as plt

def init_NeighborsClassifier(k):
    return neighbors.KNeighborsClassifier(n_neighbors=k,metric="cosine")

def fit (est, X, y):
    est.fit(X, y)

def predict (est, X):
    return tuple(est.predict(X))

def plot(acc):
    print ("k ", [v[0] for v in acc])
    print ("k ", [int(v[0]) for v in acc])
    plt.plot([int(v[0]) for v in acc], [v[1] for v in acc], c="blue")
    plt.plot([int(v[0]) for v in acc], [v[2] for v in acc], c="red")
    plt.title('malware')
    plt.xlabel('k')
    plt.ylabel('acc')
    plt.show()

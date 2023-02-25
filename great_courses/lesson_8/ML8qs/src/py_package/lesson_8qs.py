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

def hist_plot(dat1, dat2):
    plt.hist([dat1, dat2], bins=60, density=True)
    plt.gca().set(title='Malware Distance Histogram', xlabel='Distance', ylabel='Frequency');
    plt.show()

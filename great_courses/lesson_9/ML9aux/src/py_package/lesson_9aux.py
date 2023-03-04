import numpy as np

from sklearn import svm

import matplotlib
from matplotlib import pyplot as plt

def init_svc(degree):
    return svm.SVC(gamma='scale',kernel='poly', degree=degree)

def fit (est, X, y):
    est.fit(X, np.ravel(y))

def predict (est, X):
    return tuple(est.predict(X))

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

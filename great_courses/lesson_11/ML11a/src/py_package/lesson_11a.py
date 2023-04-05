import numpy as np
from matplotlib import pyplot as plt

def plot(X, Y):
    print("plot x points", X[11][0], X[12][0], X[13][0], X[14][0], X[15][0], Y[16][0], Y[17][0], Y[18][0], Y[19][0], X[20][0])
    print("plot X y points", X[11][1], X[12][1], X[13][1], X[14][1], X[15][1], Y[16][1], Y[17][1], Y[18][1], Y[19][1], X[20][1])
    print("plot Y x points", Y[11][0], Y[12][0], Y[13][0], Y[14][0], X[15][0], Y[16][0], Y[17][0], Y[18][0], Y[19][0], X[20][0])
    print("plot Y y points", Y[11][1], Y[12][1], Y[13][1], Y[14][1], X[15][1], Y[16][1], Y[17][1], Y[18][1], Y[19][1], X[20][1])
    plt.plot(X[:][0], X[:][1],'o',color='r')
    plt.plot(Y[:][0], Y[:][1],'o',color='b')
    plt.xticks(np.arange(np.min(X), np.max(X) + 20, 20.0))
    plt.yticks(np.arange(np.min(Y), np.max(Y) + 20, 20.0))
    plt.show()

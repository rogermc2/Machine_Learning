import numpy as np

from sklearn import tree
from sklearn import neighbors

import matplotlib
from matplotlib import pyplot as plt
from matplotlib import colors as cl
import matplotlib.cm as cm

def init_DecisionTreeClassifier(max_nodes):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_nodes)

def init_NeighborsClassifier(k):
    return neighbors.KNeighborsClassifier(n_neighbors=k,metric="cosine")

def fit (est, X, y):
    est.fit(X, y)

def predict (est, X):
    return tuple(est.predict(X))

def xy_plot(dat1, dat2):
    dat1_l = np.asarray (dat1)
    dat2_l = np.asarray (dat2)
    plt.scatter (dat1_l[:,1], dat1_l[:,0], color='orange')
    plt.scatter (dat2_l[:,1], dat2_l[:,0], color='cyan')
    plt.ylim(65,77)
    plt.xlim(15,90)
    plt.legend(bbox_to_anchor=(0.5, -0.15), loc="upper center", ncol=2)
    plt.title('Comfort')
    plt.xlabel('relative humidity')
    plt.ylabel('temp (F)')
    plt.tight_layout()
    plt.show()

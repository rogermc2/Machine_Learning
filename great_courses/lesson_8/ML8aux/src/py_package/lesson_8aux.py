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
    dat1_l = np.asarray (dat1[0:500])
    dat2_l = np.asarray (dat2[0:500])
    plt.scatter (dat1_l[:,1], dat1_l[:,0], color='orange')
    plt.scatter (dat2_l[:,1], dat2_l[:,0], color='cyan')
    plt.ylim(65,77)
    plt.xlim(15,90)
    plt.legend(["True", "False"], bbox_to_anchor=(0.5, -0.15), loc="upper center", ncol=2)
    plt.title('Comfort')
    plt.xlabel('relative humidity')
    plt.ylabel('temp (F)')
    plt.tight_layout()
    plt.show()

def pltcolor(lst):
    cols=[]
    for l in lst:
        if l=="False_Positive":
            cols.append('cyan')
        elif l=="False_Negative":
            cols.append('green')
        elif l=="True_Negative":
            cols.append('orange')
        elif l=="True_Positive":
            cols.append('red')
        else:
            cols.append('black')
    return cols

def plot_predictions(dat, pred):
    dat1 = np.asarray (dat)
    pred1 = list (pred)
    cols=pltcolor(pred1)
    plt.scatter (dat1[:,1], dat1[:,0], c=cols)
    plt.ylim(65,77)
    plt.xlim(15,90)
    plt.legend(["True Negative", "True Positive", "False  Negative", "False Positive"], bbox_to_anchor=(0.5, -0.15), loc="upper center", ncol=2)
    plt.title('Comfort: Decision Tree Model')
    plt.xlabel('relative humidity')
    plt.ylabel('temp (F)')
    plt.tight_layout()
    plt.show()

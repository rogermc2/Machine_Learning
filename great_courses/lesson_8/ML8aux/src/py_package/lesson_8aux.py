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
    
def split_data (data, pred):
    fp=[]
    fn=[]
    tn=[]
    tp=[]
#    data1 = np.asarray (data)
    index=-1
    for item in pred:
        index = index + 1
        coords=[data[index][0], data[index][1]]
        if item=="False_Positive":
            fp.append(coords)
        elif item=="False_Negative":
            fn.append(coords)
        elif item=="True_Negative":
            tn.append(coords)
        else:
            tp.append(coords)
    return fp, fn, tn, tp

def plot_predictions(dat, pred):
    dat1 = np.asarray (dat)
    pred1 = list (pred)
    split = split_data (dat, pred)
    fp = np.asarray(split[0])
    fn = np.asarray(split[1])
    tn = np.asarray(split[2])
    tp = np.asarray(split[3])
    plt.scatter (fp[:,1], fp[:,0])
    plt.scatter (fn[:,1], fn[:,0])
    plt.scatter (tn[:,1], tn[:,0])
    plt.scatter (tp[:,1], tp[:,0])
    
    plt.ylim(65,77)
    plt.xlim(15,90)
    plt.legend(["False Positive", "False  Negative", "True Negative", "True Positive"], bbox_to_anchor=(0.5, -0.15), loc="upper center", ncol=2)
    plt.title('Comfort: Decision Tree Model')
    plt.xlabel('relative humidity')
    plt.ylabel('temp (F)')
    plt.tight_layout()
    plt.show()

def print_tree (clf):
    feat_names=["temp","humididty"]
    tree.plot_tree (clf, feature_names=feat_names, filled=True,
                    rounded=True, fontsize=8)
    plt.show()
    print()

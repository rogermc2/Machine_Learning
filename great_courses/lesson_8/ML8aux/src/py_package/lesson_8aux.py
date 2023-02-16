import numpy as np

import pandas as pd
from sklearn import tree
from sklearn import neighbors

import matplotlib
from matplotlib import pyplot as plt
import seaborn as sns

def init_DecisionTreeClassifier(max_nodes):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_nodes)

def init_NeighborsClassifier(k):
    return neighbors.KNeighborsClassifier(n_neighbors=k,metric="cosine")

def fit (est, X, y):
    est.fit(X, y)

def predict (est, X):
    return tuple(est.predict(X))

def plot(dat, labs):
    sns.set_palette('colorblind')
    dat_df = pd.DataFrame(dat)
    labs_df = pd.DataFrame(labs)
    ax = sns.scatterplot(data=pd.concat([dat_df, labs_df], axis=1), linewidth=0, marker='o')
    plt.ylim(65,77)
    plt.xlim(15,90)
    plt.legend(bbox_to_anchor=(0.5, -0.15), loc="upper center", ncol=2)
    plt.title('Comfort')
    plt.xlabel('relative humidity')
    plt.ylabel('temp (F)')
    plt.tight_layout()
    plt.show()

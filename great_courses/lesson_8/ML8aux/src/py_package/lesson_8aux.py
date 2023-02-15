import numpy as np

import pandas as pd
from sklearn import tree
from sklearn import neighbors

import matplotlib
from matplotlib import pyplot as plt
import seaborn as sns

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
    ax = sns.scatterplot(x='relative humidity', y='temp (F)', hue='labs', data=pd.concat([dat_df, labs_df], axis=1), linewidth=0, marker='o')
#    h,l = ax.get_legend_handles_labels()
#    plt.ylim(65,77)
#    plt.xlim(15,90)
#    plt.legend(h[1:], l[1:], bbox_to_anchor=(0.5, -0.15), loc="upper center", ncol=2)
#    plt.title('Comfort')
#    plt.tight_layout()

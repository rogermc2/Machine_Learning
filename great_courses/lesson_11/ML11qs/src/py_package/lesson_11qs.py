import sklearn;
from sklearn.cluster import KMeans
from sklearn.cluster import AgglomerativeClustering
from sklearn.neighbors import KNeighborsClassifier

import numpy as np

def clust (num_clusters):
    return sklearn.cluster.KMeans(n_clusters=num_clusters)

def fit (clf, X_train):
    clf.fit(X_train)
    
def copy_labels (clf):
    return clf.labels_.copy()

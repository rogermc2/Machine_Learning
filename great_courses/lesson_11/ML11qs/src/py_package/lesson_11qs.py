import sklearn;
from sklearn.cluster import KMeans
from sklearn.cluster import AgglomerativeClustering
from sklearn.neighbors import KNeighborsClassifier

import numpy as np

def clust (num_clusters):
    return sklearn.cluster.KMeans(n_clusters=num_clusters)

def kmeans_fit (num_clusters, X):
    return KMeans(n_clusters=num_clusters,n_init="auto").fit(X)
    
def copy_labels (clf):
    return clf.labels_.copy()

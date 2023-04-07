import sklearn;
from sklearn.cluster import KMeans
from sklearn.cluster import AgglomerativeClustering
from sklearn.neighbors import KNeighborsClassifier

import numpy as np

#def clust (num_clusters):
#    return sklearn.cluster.KMeans(n_clusters=num_clusters)

def kmeans_fit (num_clusters, X):
    kmeans = KMeans(n_clusters=num_clusters).fit(X)
    return kmeans
    
def labels (kmeans):
#    print ("labels kmeans.labels_", kmeans.labels_.shape)
    print ("labels kmeans.labels_", kmeans.labels_)
    return tuple(kmeans.labels_)

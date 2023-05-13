import sklearn;
from sklearn.cluster import KMeans
from sklearn.cluster import AgglomerativeClustering
from sklearn.neighbors import KNeighborsClassifier

#import numpy as np

def kmeans_fit (num_clusters, X):
    kmeans = KMeans(n_clusters=num_clusters).fit(X)
    return kmeans
    
def labels (kmeans):
    return tuple(kmeans.labels_)

def y_pred (X_train,X_test,Y):
    clf = KNeighborsClassifier(n_neighbors=1).fit(X_train,Y)
    return tuple(clf.predict(X_test))

def plot(data, alphas):
    plt.scatter(alphas, data)
    plt.plot(alphas, data)
    plt.show()

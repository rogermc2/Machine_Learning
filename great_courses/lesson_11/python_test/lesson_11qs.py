import sys
import os
import numpy as np
import csv
from sklearn.model_selection import train_test_split

from sklearn.cluster import KMeans
from sklearn.cluster import AgglomerativeClustering
from sklearn.neighbors import KNeighborsClassifier

def get_data ():
  file = open('/Ada_Projects/machine_learning/neural_learning/datasets/mnist_784.csv')
  csvreader = csv.reader(file)
  data = []
  for row in csvreader:
          data.append(row)
  file.close()
  return np.array(data, dtype=float)

def tryclusterer(k, clust):
  # do the clustering
  clr = clust(n_clusters=k, n_init=10)
  clr.fit(X_train)
  train_ids = clr.labels_.copy()
  print ("train_ids", train_ids.shape)

  # Request one label per cluster and make an interim dataset out of X_train, y_guess .
  clust_labs = np.array(k*['-1'])
  for i in range(k):
    d = np.where(train_ids == i)
    e = y_train[d][0]
    clust_labs[i] = e[0]
    y_guess = clust_labs[train_ids]
  print ("clust_labs", clust_labs)
  print ("y_guess", y_guess.shape)

  # Assign test data labels based on the nearest instance in the interim dataset.
  clf = KNeighborsClassifier(n_neighbors=1).fit(X_train,y_guess)
  y_pred = clf.predict(X_test)
  return(sum(y_pred == y_test)/len(y_test))

data = get_data()

print ("Splitting data")
X_train, X_test, y_train, y_test = train_test_split(data [:,:-1], data [:,-1:], test_size=0.1)
X_train, X_test, y_train, y_test = train_test_split(X_test, y_test, test_size=0.33)

print ("X_train", X_train.shape)
print ("y_train", y_train.shape)

tryclusterer(50, KMeans)


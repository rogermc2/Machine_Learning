import sys
import os
import numpy as np
import csv
from sklearn.model_selection import train_test_split
from scipy import stats
import math
from functools import reduce
import random
from matplotlib import pyplot as plt

def assign_data(data,centers):
  n = len(data)
  d = len(data[0])
  # k is the number of clusters
  k = len(centers)
  # first, subtract the set of centers from each data point
  res = np.reshape(data,(1,n,d))-np.reshape(centers,(k,1,d))
  # sum the squared differences
  res2 = np.add.reduce(res**2,2)
  # assign each data point to its closest center
  centerids = np.apply_along_axis(np.argmin,0,res2)
  loss = sum(np.apply_along_axis(np.min,0,res2))
  return(centerids, loss)

def compute_means(data, centerids, k):
  n = len(data)
  d = len(data[0])
 
  centers = np.zeros(shape=(k,d))
 
  for i in range(k):
    # Gather the data points assigned to cluster i
    cols = np.array([data[j] for j in range(n) if centerids[j] == i])
    # Average to get mean for that cluster
    if len(cols) == 0: 
      centers[i] = data[random.randint(0,n-1)]
    else:
      centers[i] = cols.mean(0)
  return(centers)

def kmeans(data, k):
  n = len(data)
  d = len(data[0])
  # grab the centers from random points
  centers = data[[random.randint(0,n-1) for i in range(k)]]
  oldloss = 0
  loss = 1
  while oldloss != loss:
    oldloss = loss
    centerids, loss = assign_data(data,centers)
    centers = compute_means(data, centerids, k)
  return(centers, loss)

def get_data ():
  file = open('/Ada_Projects/machine_learning/neural_learning/datasets/mnist_784.csv')
  csvreader = csv.reader(file)
  data = []
  for row in csvreader:
          data.append(row)
  file.close()
  return np.array(data, dtype=float)

data = get_data()

print ("Splitting data")
X_train, X_test, y_train, y_test = train_test_split(data [:,:-1], data [:,-1:], test_size=0.1)
X_train, X_test, y_train, y_test = train_test_split(X_test, y_test, test_size=0.33)

print ("X_train", X_train.shape)
print ("y_train", y_train.shape)

nlabeled = 20
ans = []
k = 10
bestcenters, bestloss = kmeans(X_train, k)
print ("Initial best loss: ", bestloss)
for rep in range(1):
  centers, loss = kmeans(X_train, k)
  if loss < bestloss:
    bestcenters, bestloss = centers, loss
  print ("best loss: ", bestloss)

test_centerids, loss = assign_data(X_test, bestcenters)
# Use the labeled examples to label the clusters
train_centerids, loss = assign_data(X_train[:nlabeled], bestcenters)
labs = y_train[:nlabeled]
# print ("labs", labs.shape)
clust_labs = np.repeat(labs[0],k)
# print ("clust_labs init", clust_labs)
print ("train_centerids", train_centerids.shape)
print ("test_centerids", test_centerids.shape)
print ("test_centerids", test_centerids)

for i in range(k):
  mode = stats.mode(labs[train_centerids == i]).mode
  # print ("mode", mode)
  # print ("train_centerids == i", i, train_centerids == i)
  # print ("labs[train_centerids == i]", labs[train_centerids == i])
  if len(mode) > 0:
    clust_labs[i] = mode[0]
# print ("clust_labs", clust_labs)
ans = ans + [(k,sum(clust_labs[test_centerids] == y_test)/len(y_test))]
# print ("ans length", len (ans))
print ("test_centerids", test_centerids)
print ("clust_labs", clust_labs)
print ("clust_labs[test_centerids]", clust_labs[test_centerids])
print ("clust_labs[test_centerids] == 0", clust_labs[test_centerids] == 0,0)

# plt.plot(X_test[clust_labs[test_centerids] == 0,0],X_test[clust_labs[test_centerids] == 0,1],'o',color='r')
# plt.plot(X_test[clust_labs[test_centerids] == 1,0],X_test[clust_labs[test_centerids] == 1,1],'o',color='b')
# plt.show()
labids, loss = assign_data(X_test, X_train[:nlabeled])
print(nlabeled, sum(y_train[labids] == y_test)/len(y_test))
# tt_comp = y_train[labids] == y_test
# print("labids", len(labids))
# print("tt comp", tt_comp)
# print("tt comp", tt_comp.shape)
# print("y_test", y_test.shape)
# ttsum=sum(y_train[labids] == y_test)
# print("ttsum", ttsum)


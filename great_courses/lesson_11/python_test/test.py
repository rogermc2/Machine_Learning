import sys
import os
import numpy as np
from scipy import stats
import math
from functools import reduce
import random

print (os.getcwd())

def assign_data(data,centers):
  n = len(data)
  d = len(data[0])
  # k is the number of clusters
  k = len(centers)
  # first, subtract the set of centers from each data point
  res = np.reshape(data,(1,n,d))-np.reshape(centers,(k,1,d))
  # sum the squared differences
  res2 = np.add.reduce(res**2,2)
  # print ("res2", res2[:2][:4])
  res2diff = np.apply_along_axis(np.min,0,res2)
  print ("min vals", res2diff)
  # assign each data point to its closest center
  centerids = np.apply_along_axis(np.argmin,0,res2)
  loss = sum(np.apply_along_axis(np.min,0,res2))
  return(centerids, loss)

def compute_means(data, centerids, k):
  # n is number of data points
  n = len(data)
  d = len(data[0])
 
  centers = np.zeros(shape=(k,d))
 
  for i in range(k):
    # Gather the data points assigned to cluster i
    cols = np.array([data[j] for j in range(n) if centerids[j] == i])
    # Average to get mean for that cluster
    if len(cols) == 0: 
      centers[i] = data[i]
      # centers[i] = data[random.randint(0,n-1)]
    else:
      print ("cols shape: ", cols.shape)
      centers[i] = cols.mean(0)
  return(centers)

def kmeans(data, k):
  n = len(data)
  d = len(data[0])
  # grab the centers from random points
  centers = data[[i for i in range(k)]]
  # centers = data[[random.randint(0,n-1) for i in range(k)]]
  oldloss = 0
  loss = 1
  while oldloss != loss:
    oldloss = loss
    print ("kmeans loss: ", loss)
    centerids, loss = assign_data(data,centers)
    centers = compute_means(data, centerids, k)
  return(centers, loss)

X_train = np.asarray([[21,161,160,185,252,252,253,193,128,29],
	[0,0,0,0,188,247,65,0,0,0],
	[0,19,181,253,209,88,0,0,0,0],
	[0,19,253,254,213,118,0,0,0,0],
	[0,10,253,151,49,3,0,0,0,0],
	[0,0,3,162,214,11,0,0,0,0],
	[0,1,12,128,227,253,0,0,0,0],
	[0,5,32,115,239,254,0,0,0,0],
	[0,0,140,253,252,252,0,0,0,0],
	[0,0,94,253,252,180,0,0,0,0], 
    [0,20,243,141,59,13,0,0,0.,0],
    [0,0,13,172,204.0,21,0,0,0,0]])
Y_train = np.asarray([ 0, 2, 7, 1, 8, 6, 4, 1, 6 , 3, 2, 5])
n=len(X_train)
print ("n", n)
print ("Y", len(X_train))


nlabeled = 20
ans = []
k = 8

bestcenters, bestloss = kmeans(X_train, k)

# print ("bestcenters ", bestcenters)
print ("bestloss ", bestloss)
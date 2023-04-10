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
  # print("assign_data centers", centers)
  # first, subtract the set of centers from each data point
  res = np.reshape(data,(1,n,d))-np.reshape(centers,(k,1,d))
  # print ("assign_data res", res)
  # sum the squared differences
  res2 = np.add.reduce(res**2,2)
  # print ("res2", res2)
  # res2diff = np.apply_along_axis(np.min,0,res2)
  # print ("min vals", res2diff)
  # assign each data point to its closest center
  centerids = np.apply_along_axis(np.argmin,0,res2)
  # print ("centerids", centerids)
  loss = sum(np.apply_along_axis(np.min,0,res2))
  # print ("assign_data loss", loss)
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
      # print ("cols shape: ", cols.shape)
      centers[i] = cols.mean(0)
  # print ("compute_means centers", centers)
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
    # print ("kmeans loss: ", loss)
    # print("kmeans centers", centers)
    centerids, loss = assign_data(data,centers)
    centers = compute_means(data, centerids, k)
  return(centers, loss)

X_train = np.asarray([[1,2],
	[3,4],
	[5,6],
	[7,8],
	[9,10]])
Y_train = np.asarray([ 0, 2])
n=len(X_train)
print ("n", n)
print ("Y", len(X_train))


ans = []
k = 3

bestcenters, bestloss = kmeans(X_train, k)

print ("bestcenters ", bestcenters)
print ("bestloss ", bestloss)

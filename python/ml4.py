from sklearn.datasets import fetch_openml
from sklearn.model_selection import train_test_split
from sklearn.utils import check_random_state
import random
from sklearn import tree
import matplotlib.pyplot as plt
import numpy as np
from sklearn.neural_network import MLPClassifier
print ("ml4 loading mnist_784, takes a minute or so!")
X, y = fetch_openml('mnist_784', version=1, return_X_y=True)
print ("data loaded")
train_samples = 5000
X = np.array(X)
y = np.array(y)
print("ml4 L15 y", y)

random_state = check_random_state(0)
permutation = random_state.permutation(X.shape[0])
X = X[permutation]
y = y[permutation]
X = X.reshape((X.shape[0], -1))

X_train, X_test, y_train, y_test = train_test_split(
    X, y, train_size=train_samples, test_size=1000)
print("ml4 L25 y_train", y_train)
for i in range(1,11):
  nhidden = i*100
  clf = MLPClassifier(hidden_layer_sizes=[nhidden], max_iter = 10000, verbose = False)
  clf.fit(X_train, y_train)
  #print("ml4 L30 y_test", y_test)
  score = clf.score(X_test, y_test)
  print("nhidden, score", nhidden, score)

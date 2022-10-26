
import numpy as np
import csv
import pandas as pd

from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier

np.random.seed (8675309)

data = pd.read_csv ("../neural_learning/datasets/mnist_784.csv", header=None)
data = np.array (data)
num_cols = np.shape(data)[1]
x = data[:, :num_cols - 1]
y = data[:, - 1]
print("x size: ", x.shape)
print("y size: ", y.shape)


train_samples = 5000

# print("x[0]: ", x[0][0:200])
# print("y[0]: ", y[0])
X_train, X_test, y_train, y_test = train_test_split(
    x, y, train_size=train_samples, test_size=1000, shuffle=True)
print ("X_train[0]", X_train[0][150:160])

for i in range(1,21):
  print ("i", i)
  nhidden = i*10
  clf = MLPClassifier(hidden_layer_sizes=[nhidden], max_iter = 10000, verbose=False)
  clf.fit(X_train, y_train)
  score = clf.score(X_test, y_test)
  print(nhidden, score)

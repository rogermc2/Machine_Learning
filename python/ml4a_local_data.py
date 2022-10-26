
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

clf = MLPClassifier (hidden_layer_sizes=[], activation='identity', max_iter=1000, shuffle=False, verbose=True)
clf.fit (X_train, y_train)

# print ("X_test", X_test[:1,])
W0 = clf.coefs_[0].T
b0 = clf.intercepts_
print("W0 size: ", W0.shape)
print("W0[0][:4]: ")
print(W0[0][:4])
print("b0: ", b0)

print("X_train size: ", X_train.shape)
print("X_test size: ", X_test.shape)
score = clf.score (X_test, y_test)
print ("score", score)

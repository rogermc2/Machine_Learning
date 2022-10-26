
import numpy as np
import csv
import pandas as pd

from sklearn.neural_network import MLPClassifier

np.random.seed (8675309)
x0 = np.random.random (50) - 0.3
y0 = np.random.random (50) + 0.3
x1 = np.random.random (50) + 0.3
y1 = np.random.random (50) - 0.3
x = np.zeros ((100, 2))

x[:50, 0] = x0
x[:50, 1] = y0
x[50:, 0] = x1
x[50:, 1] = y1

y = np.array ([0]*50 + [1]*50)

idx = np.argsort (np.random.random (100))
x = x[idx]
y = y[idx]
x_train = x[:75]
x_test = x[75:]
y_train = y[:75]
y_test = y[75:]

df = pd.DataFrame(x_train)
df.to_csv('../../neural_learning/datasets/x_train.csv', index=False)
df = pd.DataFrame(x_test)
df.to_csv('../../neural_learning/datasets/x_test.csv', index=False)
df = pd.DataFrame(y_train)
df.to_csv('../../neural_learning/datasets/y_train.csv', index=False)
df = pd.DataFrame(y_test)
df.to_csv('../../neural_learning/datasets/y_test.csv', index=False)


clf = MLPClassifier (hidden_layer_sizes=(5,), shuffle=False, verbose=True)
clf.fit (x_train, y_train)

score = clf.score (x_test, y_test)
print ("score", score)
print ("x_test", x_test[:1,])
W0 = clf.coefs_[0].T
b0 = clf.intercepts_[0]
W1 = clf.coefs_[1].T
b1 = clf.intercepts_[1]
print("W0: ")
print(W0)
print("b0: ", b0)
print("W1: ", W1)
print("b1: ", b1)

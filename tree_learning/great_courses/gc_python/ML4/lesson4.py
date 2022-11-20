
from sklearn.datasets import fetch_openml
from sklearn.model_selection import train_test_split
from sklearn.utils import check_random_state
import random
from sklearn import tree
import matplotlib.pyplot as plt
import numpy as np

print ("loading mnist_784")
X, y = fetch_openml('mnist_784', version=1, return_X_y=True)
print ("mnist_784 loaded")

# X is in pandas format for some reason. Convert to numpy.
X = np.array(X)
y = np.array(y)
print ("X size: " + str (len(X)))

random_state = check_random_state(0)
permutation = random_state.permutation(X.shape[0])
X = X[permutation]
print ("X permuted size" + str (len(X)))
y = y[permutation]
print ("Y permuted")
X = X.reshape((X.shape[0], -1))

train_samples = 5000
X_train, X_test, y_train, y_test = train_test_split(
    X, y, train_size=train_samples, test_size=1000)
print ("X_train size: " + str (len(X_train)))

# i = 417
# img = np.array(X_train[i]).reshape(28,28)
# print ("image loaded")
# plt.imshow(img, cmap='gray', vmin=0, vmax=255)
# plt.show()

clf = tree.DecisionTreeClassifier(max_leaf_nodes = 170) 
clf = clf.fit(X_train, y_train)

correct = 0                     
for i in range(len(X_test)):    
  if clf.predict([X_test[i]]) == y_test[i]: correct = correct + 1
  acc = [100.0* correct / len(X_test)]
print ("X_test size: " + str (len(X_test)))
print ("accuracy: " + str (acc))


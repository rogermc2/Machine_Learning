from sklearn import tree
from gplearn.genetic import SymbolicRegressor

import matplotlib
from matplotlib import pyplot

def init_classifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)

def fit (clf, dat, labels):
    return clf.fit(dat, labels)

def predict (clf, testdat):
    return tuple (clf.predict (testdat))

def show_tree (clf, feat_names):
    tree.plot_tree (clf, feature_names=feat_names, filled=True, fontsize=8)
    pyplot.show()

def print_confusion(clf, testdat, testlabs):
    print ("Confusion matrix:")
    print(confusion_matrix(testlabs, clf.predict (testdat)))
    
def array_item (array, index):
    return array [index]
    
def matrix_item (matrix, row, col):
    return matrix [row][col]

from sklearn import tree
from sklearn.neural_network import MLPClassifier
from sklearn.naive_bayes import MultinomialNB
from sklearn.metrics import confusion_matrix

import matplotlib
from matplotlib import pyplot

def init_mlpclassifer(hidden_layer_sizes, max_iter):
    return MLPClassifier(hidden_layer_sizes=hidden_layer_sizes, max_iter=max_iter)

def init_classifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)

def fit (clf, dat, labels):
    return clf.fit(dat, labels)

def score (clf, dat, labels):
    return clf.score(dat, labels)

def multinomial_fit (dat, labels):
    return MultinomialNB().fit(dat, labels)

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

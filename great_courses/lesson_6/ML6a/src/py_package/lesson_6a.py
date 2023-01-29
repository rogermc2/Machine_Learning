from sklearn import tree

import matplotlib
from matplotlib import pyplot

def init_classifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)

def fit (clf, dat, labels):
    return clf.fit(dat, labels)

def predict (clf, testdat):
    return tuple (clf.predict (testdat))

#    return sum([yhat[i] == testlabs[i] for i in range(len(testdat))])/len(testdat)

#    correct = 0
#    for i in range(len(dat)):
#        if clf.predict([dat[i]]) == labels[i]:
#            correct = correct + 1
#    percent_correct = 100.0 * correct / len (dat)
#    print("Percentage correct: ", percent_correct)

def show_tree (clf, feat_names):
    tree.plot_tree (clf, feature_names=feat_names, filled=True,
                    rounded=True, fontsize=8)
    pyplot.show()


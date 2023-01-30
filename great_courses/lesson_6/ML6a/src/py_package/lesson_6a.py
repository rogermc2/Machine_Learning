from sklearn import tree

import matplotlib
from matplotlib import pyplot

def init_classifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)

def fit (clf, dat, labels):
    return clf.fit(dat, labels)

def predict (clf, testdat):
    return tuple (clf.predict (testdat))

def show_tree (clf, feat_names):
#    print(feat_names)
#    names=list(feat_names)
#    names = []
#    for i in range (len(feat_names)):
#        names.append (feat_names[i])
#    names = list(feat_names)
    names = ' '.join(feat_names)
    names = names[2:]
    print (names)
    print ("names length", len(names))
    tree.plot_tree (clf, feature_names=names, filled=True,
                    rounded=True, fontsize=8)
    pyplot.show()


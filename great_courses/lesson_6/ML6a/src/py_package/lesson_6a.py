from sklearn import tree

import matplotlib
from matplotlib import pyplot

def classify (dat, labels):
    
    clf = tree.DecisionTreeClassifier(max_leaf_nodes=6)
    clf = clf.fit(dat, labels)
    
#    correct = 0
#    for i in range(len(dat)):
#        if clf.predict([dat[i]]) == labels[i]:
#            correct = correct + 1
#    percent_correct = 100.0 * correct / len (dat)
#    print("Percentage correct: ", percent_correct)
#    
#    tree.plot_tree (clf, feature_names=feat_names, filled=True,
#                    rounded=True, fontsize=8)
#                    pyplot.show()
#                    print()
#                    return percent_correct


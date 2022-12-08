from sklearn import tree

import matplotlib
from matplotlib import pyplot

def lesson_3p2 (dat, labels, feat_names):
#    label_list=[]
#    for item in range(0, len(labels)):
#        label_list = label_list + [[labels [item]]]

    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
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
#    pyplot.show()
#    print()
#    return percent_correct


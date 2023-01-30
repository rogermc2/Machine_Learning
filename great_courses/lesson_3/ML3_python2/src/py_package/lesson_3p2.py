from sklearn import tree

import matplotlib
from matplotlib import pyplot

def classify (dat, lab, feat_names):
    data = dat[0]
    labels = lab[0]
    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
    clf = clf.fit(data, labels)

    correct = 0
    for i in range(len(data)):
        if clf.predict([data[i]]) == labels[i]:
            correct = correct + 1
    percent_correct = 100.0 * correct / len (data)
    print("Percentage correct: ", percent_correct)

    tree.plot_tree (clf, feature_names=feat_names, filled=True,
                    rounded=True, fontsize=8)
    pyplot.show()
    print()
    return percent_correct


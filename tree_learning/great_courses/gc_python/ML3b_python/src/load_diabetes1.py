
from sklearn import tree
import matplotlib
from matplotlib import pyplot

with open("../../diabetes.csv", "r") as f:
    data = f.readlines()

feats = data[0]
feats = feats.replace('\n', '')
feats = feats.split(",")

feature_names = feats[0:(len(feats)-1)]
dat = []
labels = []
for i in range(1, len(data)):
    line = data[i]
    line = line.replace('\n', '')
    csvline = line.split(",")
    labels = labels + [int(csvline[len(csvline)-1])]
    csvline = [float(csvline[i]) for i in range(len(csvline) - 1)]
    dat = dat + [csvline]

clf = tree._classes.DecisionTreeClassifier(max_leaf_nodes=3)
clf = clf.fit(dat, labels)

correct = 0
for i in range(len(dat)):
    if clf.predict([dat[i]]) == labels[i]: correct = correct + 1
print ("Score: ", 100.0* correct / len(dat))

tree.plot_tree (clf, feature_names=feature_names, filled=True, rounded=True)
pyplot.show()


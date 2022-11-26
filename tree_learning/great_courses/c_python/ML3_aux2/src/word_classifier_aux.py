from sklearn import tree
import matplotlib
from matplotlib import pyplot

# Putting together the data set:
# Combine these functions to load the data into multiple lists
dat = []
labels = []
words = []
pros = []
for file in ["ie", "ei"]:
    with open(file+".txt", "r") as f:
        data = f.readlines()
        for line in data:
            wordline = line.split()
            dat = dat + [features(wordline)]
            labels = labels + [["ie" in wordline[0]]]
            words = words + [wordline[0]]
            pros = pros + [wordline[1]]
# Train the decision tree classifer using eight decision rules and calculate the number of words that are correct with this model.
# Set up the learner and run it on the data then compute the accuracy and print it
# clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
clf = clf.fit(dat, labels)

correct = 0
for i in range(len(words)):
    if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
print("Number of correct words: ", correct)

feats = featurenames()
tree.plot_tree (clf, feature_names=feats,
                class_names=["ei","ie"], filled=True, rounded=True)
pyplot.show()

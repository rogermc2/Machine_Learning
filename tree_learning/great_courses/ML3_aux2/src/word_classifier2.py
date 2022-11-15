from sklearn import tree
import matplotlib
from matplotlib import pyplot

def features(wordline):
    vector = []
    if "ie" in wordline[0]: pos = wordline[0].index("ie")
    else: pos = wordline[0].index("ei")
    # pronounced as one syllable
    vector = vector + [wordline[1][pos] == '-' or wordline[1][pos+1] == '-']
    # silent
    vector = vector + [wordline[1][pos] == '-' and wordline[1][pos+1] == '-']
    # two syllables
    vector = vector + [wordline[1][pos] != '-' and wordline[1][pos+1] != '-']
    # pronunciation
    # two syllable pronunciation
    for pronounce in ["ei", "iA", "iI", "ix", "AE", "Ai", "Ax", "Ix", "iE", "ii", "yE", "yI", "ye", "yx"]:
        vector = vector + [wordline[1][pos:(pos+2)] == pronounce]
    # two syllable pronunciation
    for pronounce in ["I", "A", "E", "e", "i", "x", "a", "y", "Y"]:
        vector = vector + [wordline[1][pos:(pos+2)] == pronounce+"-" or wordline[1][pos:(pos+2)] == "-"+pronounce]
    for letter in "abcdefghijklmnopqrstuvwxyz":
        # immediate preceeding, before
        if pos > 0: vector = vector + [wordline[0][pos-1] == letter]
        else: vector = vector + [False]
        vector = vector + [letter in wordline[0][0:pos]]
        # immediate following, after
        if pos < len(wordline[0])-2: vector = vector + [wordline[0][pos+2] == letter]
        else: vector = vector + [False]
        vector = vector + [letter in wordline[0][(pos+2):]]
        # in word at all
        vector = vector + [letter in wordline[0][(pos+2):]]
    return(vector)

def featurenames():
    vector = []
    # pronounced as one syllable
    vector = vector + ["one syllable?", "silent?", "two syllables?"]
    # pronunciation
    for pro in ["ei", "iA", "iI", "ix", "AE", "Ai", "Ax", "Ix", "iE", "ii", "yE", "yI", "ye", "yx"]:
        vector = vector + ["sounds like " + pro + "?"]
    # two syllable pronunciation
    for pro in ["I", "A", "E", "e", "i", "x", "a", "y", "Y"]:
        vector = vector + ["sounds like " + pro + "?"]
    for let in "abcdefghijklmnopqrstuvwxyz":
        # immediate preceeding, before
        vector = vector + ["immediately after " + let + "?", "after " + let + "?"]
        # immediate following, after
        vector = vector + ["immediately before " + let + "?", "before " + let + "?"]
        # in word at all
        vector = vector + ["word contains " + let + "?"]
    return(vector)

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
clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
clf = clf.fit(dat, labels)

correct = 0
for i in range(len(words)):
    if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
print("Number of correct words: ", correct)

feats = featurenames()
tree.plot_tree (clf, feature_names=feats,
                class_names=["ei","ie"], filled=True, rounded=True)
pyplot.show()

# Draw the tree!

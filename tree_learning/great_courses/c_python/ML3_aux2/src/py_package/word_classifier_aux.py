from sklearn import tree

def word_classifier_aux (dat, labels):
# Train the decision tree classifer using eight decision rules and calculate the number of words that are correct with this model.
# Set up the learner and run it on the data then compute the accuracy and print it
    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
    clf = clf.fit(dat, labels)

#    correct = 0
#    for i in range(len(words)):
#        if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
#    print("Number of correct words: ", correct)
#    return (correct)

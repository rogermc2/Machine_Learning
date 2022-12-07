from sklearn import tree

import matplotlib
from matplotlib import pyplot

#dat contains the information about each word
#labels labels if the word uses ie or ei
#features is [words, pros]
#words includes each actual word, pros describes the pronouciation of each word
def word_classifier_aux (dat, labels, features, feat_names):
    label_list=[]
    for item in range(0, len(labels)):
        label_list = label_list + [[labels [item]]]

# Train the decision tree classifer using eight decision rules and calculate the number of words that are correct with this model.
# Set up the learner and run it on the data then compute the accuracy and print it
    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
    clf = clf.fit(dat, label_list)
    print("     dat[30]: ", dat[30][40:55])
    print("features[30]: ", features[30][40:55])
    print("label_list[30]: ", label_list[30])
    print ("prediction 30", clf.predict([features[30]]))
    print ("dat = features: ", dat == features)

    correct = 0
    for i in range(len(label_list)):
        if clf.predict([features[i]]) == label_list[i]:
#            if i == 30:
#                print("label_list[i]: ", i, ": ", label_list[i])
#                print("features_list[i]: ", features_list[i])
#                print("prediction: ", clf.predict([list (features_list[i])]))
            correct = correct + 1
        else:
#            print("label_list[i]: ", label_list[i])
#            print("clf.predict(features): ", clf.predict([list (features_list[i])]))
            print("incorrect")
#        if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
    print("Number of correct words: ", correct)
    # Draw the tree!
    tree.plot_tree (clf, feature_names=feat_names,
                    class_names=["ei","ie"], filled=True, rounded=True)
    pyplot.show()
    print()
    print("word_classifier_aux end")
    return correct


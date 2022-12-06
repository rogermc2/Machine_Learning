from sklearn import tree
#dat contains the information about each word
#labels labels if the word uses ie or ei
#features is [words, pros]
#words includes each actual word, pros describes the pronouciation of each word
def word_classifier_aux (dat, labels, features):
#    print("word_classifier_aux")
    data_tuple_list = list (dat)
    data_lists = list(map(list, dat))
    label_list=[]
    for item in range(0, len(labels)):
        label_list = label_list + [[labels [item]]]
    features_list = list (features)
    print("label_list length: ", len(label_list))
    print("features_list length: ", len(features_list))

# Train the decision tree classifer using eight decision rules and calculate the number of words that are correct with this model.
# Set up the learner and run it on the data then compute the accuracy and print it
    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
    clf = clf.fit(data_lists, label_list)

    correct = 0
    for i in range(len(label_list)):
        if clf.predict([list (features_list[i])]) == label_list[i]:
            correct = correct + 1
        else:
            print("label_list[i]: ", label_list[i])
            print("clf.predict(features): ", clf.predict([list (features_list[i])]))
            print("incorrect")
#        if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
    print("Number of correct words: ", correct)
    print("word_classifier_aux end")
    return correct


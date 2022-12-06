from sklearn import tree

def word_classifier_aux (dat, labels, test):
#    print("word_classifier_aux")
    data_tuple_list = list (dat)
    data_lists = list(map(list, dat))
    label_list=[]
    for item in range(0, len(labels)):
        label_list = label_list + [[labels [item]]]
    test_list = list (test)
    print("label_list length: ", len(label_list))
    print("test_list length: ", len(test_list))

# Train the decision tree classifer using eight decision rules and calculate the number of words that are correct with this model.
# Set up the learner and run it on the data then compute the accuracy and print it
    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
    clf = clf.fit(data_lists, label_list)

    correct = 0
    for i in range(len(label_list)):
        if clf.predict([list (test_list[i])]) == label_list[i]:
            correct = correct + 1
        else:
            print("label_list[i]: ", label_list[i])
            print("clf.predict(features): ", clf.predict([list (test_list[i])]))
            print("incorrect")
#        if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
    print("Number of correct words: ", correct)
    print("word_classifier_aux end")
    return correct


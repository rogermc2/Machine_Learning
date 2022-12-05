from sklearn import tree

def word_classifier_aux (dat, labels):
    print("word_classifier_aux")
    data_tuple_list = list (dat)
    data_lists = list(map(list, dat))
    label_list=[]
    for item in range(0, len(labels)):
        label_list = label_list + [[labels [item]]]
    
    print("data_tuple_list length: ", len(data_tuple_list))
    print("data_tuple_list [1][0:20]: ", data_tuple_list [1][0:20])
    print("data_lists length: ", len(data_lists))
    print("data_lists [1][0:20]: ", data_lists [1][0:20])
    
    print("label_list length: ", len(label_list))
    print("label_list: ", label_list [0:5])

# Train the decision tree classifer using eight decision rules and calculate the number of words that are correct with this model.
# Set up the learner and run it on the data then compute the accuracy and print it
#    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
#    print("clf set")
#    clf = clf.fit(data_lists, label_list)

#    correct = 0
#    for i in range(len(words)):
#        if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
#    print("Number of correct words: ", correct)
#    print("word_classifier_aux end")
    return 0


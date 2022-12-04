
from sklearn import tree

def word_classifier_aux (dat, labels):
    print("word_classifier_aux")
    data_List = list (dat)
    label_list = list (labels)
    
    print("data_List length: ", len(data_List))
    print("data_List [1]: ", data_List [1][0:20])
    
    print("label_list length: ", len(label_list))
    print("label_list: ", label_list [0:10])

# Train the decision tree classifer using eight decision rules and calculate the number of words that are correct with this model.
# Set up the learner and run it on the data then compute the accuracy and print it
    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
    print("clf set")
#    clf = clf.fit(dat, labels)

#    correct = 0
#    for i in range(len(words)):
#        if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
#    print("Number of correct words: ", correct)
#    print("word_classifier_aux end")
    return 0


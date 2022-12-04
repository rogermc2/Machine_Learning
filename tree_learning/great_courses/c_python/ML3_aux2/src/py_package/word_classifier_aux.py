
from sklearn import tree

def word_classifier_aux (dat, labels):
    print("word_classifier_aux")
    print("dat length: ", len(dat))
    print("labels length: ", len(labels))
# Train the decision tree classifer using eight decision rules and calculate the number of words that are correct with this model.
# Set up the learner and run it on the data then compute the accuracy and print it
#    clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)
#    print("clf set")
    label_list = list (labels)
    print("label_list length: ", len(labels))
    
    res = type(dat) is tuple
    print("dat is tuple: ", res)
    
    data = list (dat)
    print("data set")
    
    res = type(dat[0]) is tuple
    print("dat[0] is tuple: ", res)
    print("type(data[0]): ", type(data[0]))
    res = type(data[0]) is tuple
    print("data[0] is tuple: ", res)
#    t0=dat[0]
#    print("t0 length: ", len(t0))
#    t00=dat[0][0]
#    print("t00 length: ", len(t00))
#    words = [list(tup) for tup in dat]
#    print("clf set")
#    print("words length: ", len(words))
#    clf = clf.fit(dat, labels)

#    correct = 0
#    for i in range(len(words)):
#        if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
#    print("Number of correct words: ", correct)
#    print("word_classifier_aux end")
#    return 0


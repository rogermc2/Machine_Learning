from sklearn import tree

def predict (dat, labels):

    correct = 0
    for i in range(len(words)):
        if clf.predict([features([words[i], pros[i]])]) == labels[i]: correct = correct + 1
    print("Number of correct words: ", correct)
    print("predict end")
    return correct


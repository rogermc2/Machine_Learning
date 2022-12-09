from sklearn.neural_network import MLPClassifier

def classify (X_train, y_train):
    clf = MLPClassifier(hidden_layer_sizes=[], max_iter = 10000, activation = 'identity')
    clf = clf.fit(X_train, y_train)

    score = clf.score(X_train, y_train)
    print("Score: ", score)

    return score


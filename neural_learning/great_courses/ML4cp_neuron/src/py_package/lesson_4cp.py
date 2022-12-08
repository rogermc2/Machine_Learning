from sklearn.neural_network import MLPClassifier

def classify (X_train, y_train, X_test, y_test):
    clf = MLPClassifier(hidden_layer_sizes=[170], max_iter = 10000, activation = 'identity')
    clf = clf.fit(X_train, y_train)

    score = clf.score(X_test, y_test)
    print("Score: ", score)

    return score


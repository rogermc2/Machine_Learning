from sklearn.neural_network import MLPClassifier

def classify (X_train, y_train, X_test, y_test):
    print("classifying")
    print("Hidden  Score")
    for i in range(1,21):
        nhidden = i*10
        clf = MLPClassifier(hidden_layer_sizes=[nhidden], max_iter = 10000)
        clf = clf.fit(X_train, y_train)

        score = clf.score(X_test, y_test)
        print(nhidden, "    ", score)

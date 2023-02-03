from sklearn import tree
from sklearn.neural_network import MLPClassifier
from sklearn.naive_bayes import MultinomialNB

def init_mlpclassifer(hidden_layer_sizes, max_iter):
    return MLPClassifier(hidden_layer_sizes=hidden_layer_sizes, max_iter=max_iter)

def init_classifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)

def fit (clf, dat, labels):
    return clf.fit(dat, labels)

def score (clf, dat, labels):
    return clf.score(dat, labels)

def multinomial_fit (dat, labels):
    return MultinomialNB().fit(dat, labels)



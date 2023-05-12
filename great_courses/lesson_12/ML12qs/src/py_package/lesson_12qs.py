import numpy as np
from sklearn.naive_bayes import MultinomialNB
from matplotlib import pyplot as plt

def init_multinomial_nb(alpha):
    return MultinomialNB(alpha=alpha)

def fit (clf, features, labels):
    clf.fit(features, labels)

def predict_proba(clf, features):
    pred=clf.predict_proba(features)
    return tuple(map(tuple, pred))

def plot(alphas, result):
    plt.scatter(alphas, result)
    plt.plot(alphas, result)
    plt.xlabel("Alphas")
    plt.ylabel("Score")
    plt.show()

import numpy as np
from sklearn.naive_bayes import MultinomialNB
from matplotlib import pyplot as plt

def init_multinomialnb1():
    return MultinomialNB()

def init_multinomial_nb2(alpha):
    return MultinomialNB(alpha=alpha)

def fit (clf, features, labels):
    clf.fit(features, labels)

def predict_proba(clf, features):
    return tuple(clf.predict_proba(features))

def plot(alphas, result):
    plt.scatter(alphas, result)
    plt.plot(alphas, result)
#    plt.xticks(np.arange(0, np.max(data1) + 20, 20.0))
#    plt.yticks(np.arange(0, np.max(data1) + 20, 20.0))
    plt.show()

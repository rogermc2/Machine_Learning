import numpy as np
from sklearn.naive_bayes import MultinomialNB
from matplotlib import pyplot as plt

def multinomial_nb():
    return MultinomialNB()

def multinomial_nb(alpha):
    return MultinomialNB(alpha=alpha)

def fit (clf, features):
    clf.fit(features)

def fit (clf, features, labels):
    clf.fit(features, labels)

def predict_proba(clf, features):
    return tuple(clf.predict_proba(features))

def plot(data1, data2):
    dp1 = np.array (data1)
    dp2 = np.array (data2)
    plt.plot(dp1[:,0], dp1[:,1],'o',color='r')
    plt.plot(dp2[:,0], dp2[:,1],'o',color='b')
    plt.xticks(np.arange(0, np.max(data1) + 20, 20.0))
    plt.yticks(np.arange(0, np.max(data1) + 20, 20.0))
    plt.show()

import numpy as np
import tensorflow as tf
from sklearn.datasets import fetch_20newsgroups
from sklearn.model_selection import train_test_split
#from keras.utils import to_categorical
from keras.layers import Dense, Input, GlobalMaxPooling1D
from keras.layers import Conv1D, MaxPooling1D, Embedding
from keras.models import Model
from keras.initializers import Constant
from matplotlib import pyplot as plt

def fetch_newsgroups():
    print ("fetch_newsgroups")
    groups = ['rec.sport.baseball', 'rec.sport.hockey']
    newsgroups = fetch_20newsgroups(subset='all', remove = ['headers', 'footers', 'quotes'], categories = groups)
    print ("fetch_newsgroups newsgroups loaded")
    print ("newsgroups", newsgroups.__dir__())
    print("newsgroups dir", dir())
    print("newsgroups type", type(newsgroups))
    print("newsgroups length", len(newsgroups))
    print("newsgroups length['data']", len(newsgroups['data']))
#    print("newsgroups length[1]", len(newsgroups['filenames']))
    print("newsgroups['data'] type", type(newsgroups['data']))
#    print("newsgroups['filenames'] type", type(newsgroups['filenames']))
    print("newsgroups['data'][0] ", newsgroups['data'][0])
    print("newsgroups['data'][0] type", type(newsgroups['data'][0]))
    print("newsgroups length['data'][0]", len(newsgroups['data'][0]))
    print("newsgroups['data'][0][0]", newsgroups['data'][0][0])
    print("newsgroups['data'][0][1]", newsgroups['data'][0][1])
    print("newsgroups['data'][0][2]", newsgroups['data'][0][2])
    print("newsgroups['data'][0][0:40]", newsgroups['data'][0][0:40])
    print("newsgroups['data'][0][0] type", type(newsgroups['data'][0][0]))
    print("newsgroups['data'][0][0] length", len(newsgroups['data'][0][0]))
    return (tuple(map (tuple, newsgroups['data'])), tuple(map (tuple, newsgroups['filenames'])))

def init_multinomialnb1():
    return MultinomialNB()

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

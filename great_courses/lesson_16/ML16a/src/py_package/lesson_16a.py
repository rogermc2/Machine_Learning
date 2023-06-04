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
    print ("fetching newsgroups")
    groups = ['rec.sport.baseball', 'rec.sport.hockey']
    newsgroups = fetch_20newsgroups(subset='all', remove = ['headers', 'footers', 'quotes'], categories = groups)
    print ("fetch_newsgroups newsgroups loaded")
    print ("newsgroups", newsgroups.__dir__())
    return (tuple(map (tuple, newsgroups['data'])), tuple(map (tuple, newsgroups['filenames'])), tuple(map (tuple, newsgroups['target_names'])))

def init_tokenizer(max_words):
    return tf.keras.preprocessing.text.Tokenizer(num_words=max_words)

def fit (tokenizer, newsgroups_data):
    tokenizer.fit_on_texts(newsgroups_data)

def get_sequences (tokenizer, newsgroups_data):
    return tuple (map(tuple,tokenizer.texts_to_sequences(newsgroups_data)))

def get_word_index (tokenizer):
    print("get_word_index word_index type: ", type(tokenizer.word_index))
    return tuple (tokenizer.word_index)

def get_data (sequences, max_sequence_length):
    return tuple (tf.keras.preprocessing.sequence.pad_sequences(sequences, maxlen=max_sequence_length))
    
def predict_proba(clf, features):
    pred=clf.predict_proba(features)
    return tuple(map(tuple, pred))

def plot(alphas, result):
    plt.scatter(alphas, result)
    plt.plot(alphas, result)
    plt.xlabel("Alphas")
    plt.ylabel("Score")
    plt.show()

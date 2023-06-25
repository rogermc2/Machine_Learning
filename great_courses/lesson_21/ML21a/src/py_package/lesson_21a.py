
import numpy as np
import math
import tensorflow as tf
from sklearn.model_selection import train_test_split
from matplotlib import pyplot as plt
import matplotlib.colors as mcolors

def init_tokenizer(max_words):
    return tf.keras.preprocessing.text.Tokenizer(num_words=max_words)

def fit (tokenizer, newsgroups_data):
    tokenizer.fit_on_texts(newsgroups_data)

def get_sequences (tokenizer, newsgroups_data):
    return tuple (map(tuple,tokenizer.texts_to_sequences(newsgroups_data)))

def get_word_index (tokenizer):
    tuple_list = [(key, value) for key, value in tokenizer.word_index.items()]
    return tuple (tuple_list)


def get_labels (sequences, max_sequence_length):
    sequences_list =[]
    for item in sequences:
        sequences_list.append(list(item))
    return tf.keras.preprocessing.sequence.pad_sequences(sequences_list, maxlen=max_sequence_length)
    
def predict_proba(clf, features):
    pred=clf.predict_proba(features)
    return tuple(map(tuple, pred))

def plot_matrix(matrix):
    mat = np.asarray(matrix)
    colors = ['b', 'g', 'r', 'c', 'm', 'y', 'k', 'w']
    fig = plt.figure()
    ax = plt.subplot(111)
    ax.set_facecolor('#333')
    ax.spines.right.set_visible(False)
    ax.spines.top.set_visible(False)
    xvals=range(len(mat)+1)
    yvals=range(len(mat[0])+1)
    x=-1
    for row in mat:
        x=x+1
        y=-1
        for col in row:
            y=y+1
            plt.scatter(xvals[x], yvals[y], color=colors[col])
#    plt.annotate(f"({r}, {g}, {b})", (100, y[-1]))
    xticks_list = range(math.floor(min(xvals)), math.ceil(max(xvals))+1)
    plt.xticks(xticks_list)
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Grid Map")
    plt.show()

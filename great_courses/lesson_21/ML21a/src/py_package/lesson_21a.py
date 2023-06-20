
import numpy as np
import tensorflow as tf
from sklearn.model_selection import train_test_split
#from keras.utils import to_categorical
#from keras.layers import Dense, Input, GlobalMaxPooling1D
#from keras.layers import Conv1D, MaxPooling1D, Embedding
#from keras.models import Model
#from keras.initializers import Constant
from matplotlib import pyplot as plt

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
    matrix = np.array(map(np.array, matrix))
    plt.xlabel("Alphas")
    plt.ylabel("Score")
    plt.show()

import tensorflow as tf
from keras.datasets import mnist
from keras.utils import np_utils

def version():
    print("TensorFlow version:", tf.__version__)

def preprocess_data(x, y, limit):
    # reshape and normalize input data
    x = x.reshape(x.shape[0], 28 * 28, 1)
    x = x.astype("float32") / 255
    # encode output which is a number in range [0,9] into a vector of size 10
    # e.g. number 3 will become [0, 0, 0, 1, 0, 0, 0, 0, 0, 0]
    y = np_utils.to_categorical(y)
    y = y.reshape(y.shape[0], 10, 1)
    return x[:limit], y[:limit]

def load_data(num_train, num_test):
    mnist = tf.keras.datasets.mnist
    (x_train, y_train), (x_test, y_test) = mnist.load_data()
    data =[[],[],[],[]]
    data [0], data [2] = preprocess_data(x_train, y_train, num_train)
    data [1], data [3] = preprocess_data(x_test, y_test, num_test)
    #x_train, y_train = preprocess_data(x_train, y_train, 1000)
    #x_test, y_test = preprocess_data(x_test, y_test, 20)
    
    return tuple(tuple(list) for list in data)


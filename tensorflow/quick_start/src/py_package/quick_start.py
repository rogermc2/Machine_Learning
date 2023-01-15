
import sys

import tensorflow as tf
from keras.datasets import mnist
from keras.utils import np_utils

def version():
    print("Python version: ", sys.version)
    print("TensorFlow version:", tf.__version__)

def to_tuple(a):
    try:
        return tuple(to_tuple(i) for i in a)
    except TypeError:
        return a

def preprocess_data(x, y, limit):
    # reshape and normalize input data
    x = x.reshape(x.shape[0], 28 * 28, 1)
    x = x.astype("float32") / 255
    # encode output which is a number in range [0,9] into a vector of size 10
    # e.g. number 3 will become [0, 0, 0, 1, 0, 0, 0, 0, 0, 0]
    y = np_utils.to_categorical(y)
    y = y.reshape(y.shape[0], 10, 1)
    x=x[:limit]
    y=y[:limit]
    return x, y

def load_data(num_train, num_test):
    mnist = tf.keras.datasets.mnist
    (x_train, y_train), (x_test, y_test) = mnist.load_data()
    data0, data1 = preprocess_data(x_train, y_train, num_train)
    data2, data3 = preprocess_data(x_test, y_test, num_test)
    data0 = to_tuple (data0)
    data1 = to_tuple (data1)
    data2 = to_tuple (data2)
    data3 = to_tuple (data3)
#    print ("data 0 length", len (data0))
#    print ("data 1 length", len (data1))
#    print ("data 2 length", len (data2))
#    print ("data 3 length", len (data3))
#    print ("data 0 is tuple", isinstance(data0, tuple))
#    print ("data0[0] type", type(data0[0]))
#    print ("data 0[0] is tuple", isinstance(data0[0], tuple))
#    print ("data 0[0][0] is tuple", isinstance(data0[0][0], tuple))

    return data0, data1, data2, data3


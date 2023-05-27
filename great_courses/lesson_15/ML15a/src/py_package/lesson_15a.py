import numpy as np
from PIL import Image
from sklearn import tree
from matplotlib import pyplot as plt

import keras
from keras.models import Sequential
from keras.layers import Dense, Activation, Dropout, Flatten
from keras.layers import Conv2D
from keras.layers import MaxPooling2D

def load_image (file_name):
    img = Image.open(file_name)
    img_arr = np.array(img)
    img_list = []
    for i in range(len(img_arr)):
        img_2d  = tuple(map(tuple, map(tuple,img_arr[i])))
        img_list.append(img_2d)
    img.close()
    return tuple(img_list);

def predict(clf, features):
    return tuple(clf.predict(features))

def plot(clf):
    feats = ["holding", "dealer", "action", "ace"]
    tree.plot_tree (clf, feature_names=feats, filled=True,
                    rounded=True, fontsize=8)
    plt.show()

def show_bitmap(bitmap, Row_Size):
    bm_array = np.asarray(bitmap)
    num_rows = int (len(bitmap) / Row_Size)
    bm_array = bm_array.reshape(num_rows, Row_Size, 3)
    plt.imshow(bm_array)
    plt.show()
def build_network():
    input_shape = (64, 64, 3)
    #Instantiate an empty model
    model = Sequential()
    model.add(Conv2D(64, (3, 3), input_shape=input_shape, padding='same', activation='relu'))
    model.add(Conv2D(64, (3, 3), padding='same', activation='relu'))
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    model.add(Conv2D(128, (3, 3), padding='same', activation='relu'))
    model.add(Conv2D(128, (3, 3), padding='same', activation='relu'))
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    model.add(Conv2D(256, (3, 3), padding='same', activation='relu'))
    model.add(Conv2D(256, (3, 3), padding='same', activation='relu'))
    model.add(Conv2D(256, (3, 3), padding='same', activation='relu'))
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    model.add(Conv2D(512, (3, 3), padding='same', activation='relu'))
    model.add(Conv2D(512, (3, 3), padding='same', activation='relu'))
    model.add(Conv2D(512, (3, 3), padding='same', activation='relu'))
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    model.add(Conv2D(512, (3, 3), padding='same', activation='relu'))
    model.add(Conv2D(512, (3, 3), padding='same', activation='relu'))
    model.add(Conv2D(512, (3, 3), padding='same', activation='relu'))
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    model.add(Flatten())
    model.add(Dense(4096, activation='relu'))
    model.add(Dense(4096, activation='relu'))
    model.add(Dense(1, activation='sigmoid'))
    return(model)

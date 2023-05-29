import sys
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

def flatten(atuple, count):
    count=count+1
    print(count)
    if isinstance(atuple, tuple) and len(atuple) == 2 and not isinstance(atuple[0], tuple):
        res = [atuple]
        return tuple(res)
 
    res = []
    for sub in atuple:
        res += flatten(sub, count)
    return tuple(res)
    
def fit(model, X_train,Y_train, X_test,Y_test):
    print("fit X_train length", len(X_train))
    print("fit X_train[0] length", len(X_train[0]))
    print("fit X_train[0][0] length", len(X_train[0][0]))
    print("fit X_train[0][0][0] length", len(X_train[0][0][0]))
    print("fit X_train [0][0][0] length printed")

    sys.setrecursionlimit(14000)
    print("flatten recursionlimit set")
    flat = flatten(X_train, 0)
    X3=X_train[0][0][0]
    print("fit X3 type", type(X3))
    print("fit X3 length", len(X3))
    print("fit X3", X3)
    nparray=np.asarray(X3)
    print("fit nparray", nparray)
    print("fit X_train[0][0][0] type", type(X_train[0][0][0]))
    print("fit X_train[0][0][0] asarray", np.asarray(X_train[0][0][0]).shape)
    print("fit X_train[0][0] asarray", np.asarray(X_train[0][0]).shape)
    model.fit(np.asarray(X_train), np.asarray(Y_train), validation_data=(np.asarray(X_test),np.asarray(Y_test)))
    print("fit done")

def predict(clf, features):
    return tuple(clf.predict(features))

#def plot(clf):
#    feats = ["holding", "dealer", "action", "ace"]
#    tree.plot_tree (clf, feature_names=feats, filled=True,
#                    rounded=True, fontsize=8)
#    plt.show()

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

def compile(model):
    model.compile(loss='mean_squared_error', optimizer='adam', metrics=['accuracy'])

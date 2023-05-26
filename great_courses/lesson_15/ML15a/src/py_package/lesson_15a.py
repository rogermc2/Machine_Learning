import numpy as np
from PIL import Image
from sklearn import tree
from matplotlib import pyplot as plt

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

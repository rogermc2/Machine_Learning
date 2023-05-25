import numpy as np
from PIL import Image
from sklearn import tree
from matplotlib import pyplot as plt

def load_image (file_name):
    print("load_image file_name", file_name)
    img = Image.open(file_name)
    img_arr = Image.img_to_array(img)
    img_arr = img_arr.flatten()
    img_arr = img_arr.reshape(64,64,3)
    return img_arr;

def predict(clf, features):
    return tuple(clf.predict(features))

def plot(clf):
    feats = ["holding", "dealer", "action", "ace"]
    tree.plot_tree (clf, feature_names=feats, filled=True,
                    rounded=True, fontsize=8)
    plt.show()

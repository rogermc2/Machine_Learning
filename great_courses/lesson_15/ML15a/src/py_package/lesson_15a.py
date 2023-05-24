import numpy as np
import gym
from sklearn import tree
from matplotlib import pyplot as plt

def load_image (file_name):
    img = image.load_img(file_name)
    img_arr = image.img_to_array(img)
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

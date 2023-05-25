import numpy as np
from PIL import Image
from sklearn import tree
from matplotlib import pyplot as plt

def load_image (file_name):
    img = Image.open(file_name)
    print("load_image file opened")
    img_arr = np.array(img)
    img_tuple = ()
    test = tuple(map(tuple, map(tuple,img_arr[0])))
    print ("test length", len(test), len(test[0]))
    for i in range(len(img_arr)):
        img_tuple = img_tuple + tuple(map(tuple,img_arr[i]))
    print ("img_arr", img_arr.shape)
    print ("img_tuple length", len(img_tuple))
    print ("img_tuple[0] length", len(img_tuple[0]))
#    print ("img_tuple[0][0] length",  len(img_tuple[0][0]))
    return img_tuple;

def predict(clf, features):
    return tuple(clf.predict(features))

def plot(clf):
    feats = ["holding", "dealer", "action", "ace"]
    tree.plot_tree (clf, feature_names=feats, filled=True,
                    rounded=True, fontsize=8)
    plt.show()

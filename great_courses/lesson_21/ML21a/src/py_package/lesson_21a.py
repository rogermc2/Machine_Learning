
import numpy as np
import math
import tensorflow as tf
import keras.backend as K
import seaborn as sns
from sklearn.model_selection import train_test_split
from matplotlib import pyplot as plt
import matplotlib.colors as mcolors

def softmax(beta_Q):
    bq=np.asarray(beta_Q)
    return tuple(map (tuple, K.softmax(bq)))
    
def plan():
    planner = K.function([rk], [pi, Q])
    r = np.array([0, -1, -1, -1, 10])
    piout, Qout = planner([r])

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
    
def plot(grid_map):
    mat = np.asarray(grid_map)
    colors = ["white", "blue", "orange", "yellow", "green"]
    sns.heatmap(grid_map, cmap=sns.xkcd_palette(colors), yticklabels=False, xticklabels=False,
            annot=False, cbar = False, annot_kws={"size": 30}, linewidths=1, linecolor="gray")
    plt.title("Grid Map")
    plt.show()

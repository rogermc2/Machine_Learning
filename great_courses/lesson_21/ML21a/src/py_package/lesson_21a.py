
import numpy as np
import math
import tensorflow as tf
import keras.backend as K
#import seaborn as sns
from sklearn.model_selection import train_test_split
from matplotlib import pyplot as plt
import matplotlib.colors as mcolors
    
def policy(r, matmap, mattrans):
    mmap=np.asarray(matmap)
    trans=np.asarray(mattrans)
    print("mmap", mmap.shape)
    print("trans", trans.shape)
    rk = K.placeholder(len(r))
    # rfk (5 x 10 x 1) maps each location to its reward value
    rfk = K.dot(K.constant(mmap),K.reshape(rk,(-1,1)))
    # rffk (50 x 1)
    rffk = K.reshape(rfk,(-1,1))
    # v (50 x 1)
    v = K.reshape(rfk,(-1,1))
    gamma = 0.90  # future reward discount compared to current reward
    beta = 10.0
#    For each step of looking into the future, each of the five action
#    calucates an estimate of the value of taking each action.
#    Each value of q0 .. q4 corresponds to the estimated value of
#    actions 0 to 4, by multipling the transition matrix for an action
#    by the value estimate, v.
    for _ in range(50):
        q0 = K.dot(K.constant(trans[0]),v)
        q1 = K.dot(K.constant(trans[1]),v)
        q2 = K.dot(K.constant(trans[2]),v)
        q3 = K.dot(K.constant(trans[3]),v)
        q4 = K.dot(K.constant(trans[4]),v)
        Q = K.concatenate([q0,q1,q2,q3,q4])
        pi = K.softmax(beta*Q)
        v = rffk + gamma * K.reshape(K.sum(Q * pi,axis=1),(-1,1))
    planner = K.function([rk], [pi, Q])
    r = np.array([0, -1, -1, -1, 10])
    piout, Qout = planner([r])
    return (tuple (map (tuple, piout)), tuple(map (tuple, Qout)))

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

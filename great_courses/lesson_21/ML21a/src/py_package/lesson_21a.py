
import numpy as np
import math
import tensorflow as tf
import keras.backend as K
import seaborn as sns
from sklearn.model_selection import train_test_split
from matplotlib import pyplot as plt
import matplotlib.colors as mcolors

#def softmax(beta_Q):
#    bq=np.asarray(beta_Q)
#    return tuple(map (tuple, K.softmax(bq)))
#    
#def place_holder(r):
#    print("place_holder", len(r))
#    return K.placeholder(len(r))
    
def policy(r, matmap, mattrans):
    rk = K.placeholder(len(r))
    rfk = K.dot(K.constant(np.asaarray(matmap)),K.reshape(rk,(-1,1)))
    rffk = K.reshape(rfk,(-1,1))
     
    v = K.reshape(rfk,(-1,1))
    gamma = 0.90
    beta = 10.0
    
    trans=np.asaarray(mattrans)
    for _ in range(50):
        q0 = K.dot(K.constant(trans[0]),v)
        q1 = K.dot(K.constant(trans[1]),v)
        q2 = K.dot(K.constant(trans[2]),v)
        q3 = K.dot(K.constant(trans[3]),v)
        q4 = K.dot(K.constant(trans[4]),v)
        Q = K.concatenate([q0,q1,q2,q3,q4])
        pi = K.softmax(beta*Q)
        v = rffk + gamma * K.reshape(K.sum(Q * pi,axis=1),(-1,1))
    return tuple(Q, pi, v)
    
def plan(rk, pi, Q):
    planner = K.function([rk], [np.asarray (pi), np.asarray (Q)])
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

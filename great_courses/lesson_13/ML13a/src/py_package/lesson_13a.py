import numpy as np
import gym
from sklearn import tree
import graphviz
from matplotlib import pyplot as plt

def init_gym(game):
    return gym.make(game, render_mode="rgb_array")
    
def train(dat, lab):
  clf = tree.DecisionTreeRegressor(max_leaf_nodes = 6)
  clf = clf.fit(dat, lab)
  return(clf)
  
def reset(env):
    return tuple (env.reset())

def sample(env):
    return env.action_space.sample()

def step(env, action):
    return env.step(action)

def close(env):
    env.close()

def predict(clf, features):
    pred=clf.predict(features)
    return tuple(pred)

#def graph (clf):
#    dot_data = tree.export_graphviz(clf, feature_names = ["holding", "dealer", "action"], filled=True, rounded=True)
#    return graphviz.Source(dot_data)
#
#def show_graph(graph):
#    graph

def plot(clf):
    feats = ["holding", "dealer", "ace", "action"]
    tree.plot_tree (clf, feature_names=feats, filled=True,
                    rounded=True, fontsize=8)
    plt.show()

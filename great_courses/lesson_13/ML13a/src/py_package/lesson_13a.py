import numpy as np
import gym
from sklearn import tree
from matplotlib import pyplot as plt

def init_gym(game):
    return gym.make(game)
    
def train(dat, lab):
  clf = tree.DecisionTreeRegressor(max_leaf_nodes = 6)
  return(clf.fit(dat, lab))
  
def reset(env):
    return tuple (env.reset()[0])

def sample(env):
    return env.action_space.sample()

def step(env, action):
    return tuple(env.step(action))

def close(env):
    env.close()

def predict(clf, features):
    return tuple(clf.predict(features))

def plot(clf):
    feats = ["holding", "dealer", "action", "ace"]
    tree.plot_tree (clf, feature_names=feats, filled=True,
                    rounded=True, fontsize=8)
    plt.show()

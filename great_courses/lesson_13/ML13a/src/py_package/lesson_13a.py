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
    return tuple (env.reset())

def sample(env):
    return env.action_space.sample()

def step(env, action):
    return env.step(action)

def close(env):
    env.close()

def predict(clf, features):
    pred=clf.predict(features)
    print("predict pred", pred)
    return tuple(pred)

def plot(clf):
    feats = ["holding", "dealer", "ace", "action"]
    tree.plot_tree (clf, feature_names=feats, filled=True,
                    rounded=True, fontsize=8)
    plt.show()

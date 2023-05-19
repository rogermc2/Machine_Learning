import numpy as np
import gym
from sklearn import tree
from matplotlib import pyplot as plt

def init_gym(game):
    return gym.make(game, render_mode="rgb_array")

#def init_decision_tree_regressor(max_leaf_nodes):
#    return tree.DecisionTreeRegressor(max_leaf_nodes=max_leaf_nodes)
    
def train(dat, lab):
  clf = tree.DecisionTreeRegressor(max_leaf_nodes = 6)
  clf = clf.fit(dat, lab)
  return(clf)
  
def reset(env):
    env.reset()

def sample(env):
    return env.action_space.sample()

def step(env, action):
    return env.step(action)

def close(env):
    env.close()

#def fit (clf, features, labels):
#    clf.fit(features, labels)

def predict(clf, features):
    pred=clf.predict(features)
    return tuple(pred)

def render(env):
    return env.render()

def plot(env_screen):
    plt.imshow(env_screen)

import numpy as np
import gym
import gym_toytext
from sklearn import tree
from matplotlib import pyplot as plt

def init_gym(game):
    return gym.make(game)

def action(env):
    return env.action_space.sample()
    
def train(dat, lab):
  clf = tree.DecisionTreeRegressor(max_leaf_nodes = 6)
  return(clf.fit(dat, lab))
  
def reset(env):
    print ("reset(env)", tuple (env.reset())[0])
    return tuple (env.reset())[0]

def step(env, action):
    return tuple(env.step(action))

def close(env):
    env.close()

def predict(clf, observation):
    return clf.predict([(observation, a) for a in range(env.action_space.n)])

def plot(clf):
    feats = ["state", "action"]
    tree.plot_tree (clf, feature_names=feats, filled=True,
                    rounded=True, fontsize=8)
    plt.show()

import numpy as np
import gym
from sklearn import tree
from matplotlib import pyplot as plt

def init_gym(game):
    return gym.make(game)

def reset(env):
    env.reset()

def step(env, action):
    observation, reward, done, info = env.step(action)
    return done

def fit (clf, features, labels):
    clf.fit(features, labels)

def predict_proba(clf, features):
    pred=clf.predict_proba(features)
    return tuple(map(tuple, pred))

def plot(alphas, result):
    plt.scatter(alphas, result)
    plt.plot(alphas, result)
    plt.xlabel("Alphas")
    plt.ylabel("Score")
    plt.show()

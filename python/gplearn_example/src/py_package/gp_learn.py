import numpy as np

from gplearn.genetic import SymbolicRegressor
from sklearn.utils.random import check_random_state

import matplotlib
from matplotlib import pyplot as plt

def init_SymbolicRegressor():
    return SymbolicRegressor(population_size=5000,
                              generations=20, stopping_criteria=0.01,
                              p_crossover=0.7, p_subtree_mutation=0.1,
                              p_hoist_mutation=0.05, p_point_mutation=0.1,
                              max_samples=0.9, verbose=1,
                              parsimony_coefficient=0.01, random_state=0)
    
def fit (est_gp, X_train, y_train):
#def fit (est_gp):
    print ("est_gp.fit")
    est_gp.fit(X_train, y_train)

#def plot_data(X_train, y_train):
def plot_data():
    print ("plot_data")
    x0 = np.arange(-1, 1, 1/10.)
    x1 = np.arange(-1, 1, 1/10.)
    x0, x1 = np.meshgrid(x0, x1)
    y_truth = x0**2 - x1**2 + x1 - 1

    ax = plt.figure().add_subplot(projection='3d')
    ax.set_xlim(-1, 1)
    ax.set_ylim(-1, 1)
    surf = ax.plot_surface(x0, x1, y_truth, rstride=1, cstride=1,
                           color='green', alpha=0.5)
    plt.show()

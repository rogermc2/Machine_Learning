import numpy as np

from sklearn import tree
from gplearn.genetic import SymbolicRegressor

import matplotlib
from matplotlib import pyplot

def init_treeclassifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)

def init_SymbolicRegressor(pop_size, pars):
    return SymbolicRegressor(population_size=pop_size,parsimony_coefficient=pars,
                             function_set=('add', 'mul'),verbose=1)
def fit (est, X_train, y_train):
    est.fit(X_train, y_train)

def plot_data(X_train, y_train):
    pyplot.scatter(X_train, y_train)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

import numpy as np

from sklearn import tree
from gplearn.genetic import SymbolicRegressor

import matplotlib
from matplotlib import pyplot

def init_treeclassifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)
    
# init_SymbolicRegressor fails if arguments are used
def init_SymbolicRegressor():
    return SymbolicRegressor(population_size=10000,parsimony_coefficient=0.1,
                             function_set=('add', 'mul'),verbose=1)
def fit (est, X_train, y_train):
    est.fit(X_train, y_train)

def plot_data(X_train, y_train):
    pyplot.scatter(X_train, y_train)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

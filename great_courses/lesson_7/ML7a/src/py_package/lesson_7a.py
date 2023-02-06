import numpy as np

from sklearn import tree
from gplearn.genetic import SymbolicRegressor

import matplotlib
from matplotlib import pyplot

def init_treeclassifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)

def init_SymbolicRegressor(pop_size, pars):
    return SymbolicRegressor(population_size=pop_size,parsimony_coefficient=pars,
                             function_set=('add', 'mul'),verbose=1,generations=3)
def fit (est, X_train, y_train):
    est.fit(X_train, y_train)

def predict (est, X):
    print ("predict X", X[:3])
    print ("est.predict X", est.predict(X)[:3])
    return est.predict(X)

def plot_data(X_train, y_train):
    pyplot.scatter(X_train, y_train)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

def plot_prediction(X, y, X_lots, y_gp):
    pyplot.scatter(X, y)
    pyplot.plot(X_lots, y_gp)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

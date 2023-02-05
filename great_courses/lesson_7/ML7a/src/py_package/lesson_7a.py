import numpy as np

from sklearn import tree
from gplearn.genetic import SymbolicRegressor

import matplotlib
from matplotlib import pyplot

def init_classifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)
    
def init_SymbolicRegressor(pop_size, parsimony_coeff):
    return SymbolicRegressor(population_size=pop_size, parsimony_coefficient=parsimony_coeff, function_set = ('add', 'mul'))
    
def fit (est_gp, X_train, y_train):
    Xa = np.asarray (X_train)
    ya = np.asarray (y_train)
    print ("est_gp.fit")
    print ("Xa", Xa [:6])
    print ("ya", ya [:6])
    print ("X_train type", type(X_train))
    print ("y_train type", type(y_train))
    print ("X_train length", Xa.shape)
    print ("y_train length", ya.shape)
    est_gp.fit(Xa, ya)

def plot_data(X_train, y_train):
    pyplot.scatter(X_train, y_train)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

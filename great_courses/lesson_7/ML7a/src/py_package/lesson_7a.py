import numpy as np

from sklearn import tree
from gplearn.genetic import SymbolicRegressor

import matplotlib
from matplotlib import pyplot

def init_treeclassifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)
    
def init_SymbolicRegressor(pop_size, parsimony_coeff):
    return SymbolicRegressor(population_size=pop_size, parsimony_coefficient=parsimony_coeff, function_set = ('add', 'mul'))
    
def fit (est_gp, X_train, y_train):
    Xa = np.asarray (X_train)
    ya = list (y_train)
    print ("est_gp.fit")
    print ("Xa", Xa [:4])
    print ("ya", ya [:4])
    print ("Xa type", type(Xa))
    print ("ya type", type(ya))
    print ("X_train shape", Xa.shape)
    est_gp.fit(Xa, ya)

def plot_data(X_train, y_train):
    pyplot.scatter(X_train, y_train)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

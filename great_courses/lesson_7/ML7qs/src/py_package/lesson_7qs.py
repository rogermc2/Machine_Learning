import numpy as np

from gplearn.genetic import SymbolicRegressor

import matplotlib
from matplotlib import pyplot

def init_SymbolicRegressor(pop_size, pars):
    return SymbolicRegressor(population_size=pop_size,parsimony_coefficient=pars,
                             function_set=('add', 'mul','abs'),verbose=1)
def fit (est, X_train, y_train):
    est.fit(X_train, y_train)

def predict (est, X):
    return tuple(est.predict(X))

def plot_prediction(X, y, X_lots, y_gp):
    pyplot.scatter(X, y)
    pyplot.plot(X_lots, y_gp)
    pyplot.title('Function')
    pyplot.xlabel('Y')
    pyplot.ylabel('X')
    pyplot.show()

def print_program (est_gp):
    print ("genetic program:")
    print("  ", est_gp._program)

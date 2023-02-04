from sklearn import tree
from gplearn.genetic import SymbolicRegressor

import matplotlib
from matplotlib import pyplot

def init_classifer(max_leaves):
    return tree.DecisionTreeClassifier(max_leaf_nodes=max_leaves)

def plot_data(X_train, y_train):
    plt.scatter(X_train, y_train)
    plt.title('Target distance')
    plt.xlabel('angle')
    plt.ylabel('distance')
    plt.show()

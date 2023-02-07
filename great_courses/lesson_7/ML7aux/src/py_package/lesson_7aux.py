import matplotlib
from matplotlib import pyplot

def plot_xy(X_List, Y_List):
    pyplot.plot(X_List, Y_List)

def show_plot(X, Y):
    pyplot.scatter([X], [Y])
    pyplot.show()

def plot_data(angles, landing):
    pyplot.scatter(angles, landing)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

def plot_launcher(X, Y):
    pyplot.scatter(X, Y)
    pyplot.title('Launcher position')
    pyplot.xlabel('X')
    pyplot.ylabel('Y')
    pyplot.show()

def plot_values(angles, landing, population, vals):
    pyplot.scatter(angles, landing)
    pyplot.scatter(population, vals)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

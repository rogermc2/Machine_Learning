import matplotlib
from matplotlib import pyplot

def plot_data(angles, landing):
    pyplot.scatter(angles, landing)
    pyplot.title('Target distance')
    pyplot.xlabel('angle')
    pyplot.ylabel('distance')
    pyplot.show()

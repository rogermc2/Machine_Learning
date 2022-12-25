import numpy as np
import matplotlib.pyplot as plt

def show_contours(x, y, z):
    plt.contourf(x, y, z, 10, cmap="Greys")
    # k means black
    plt.contour(x, y, z, colors='k', linewidths=1)
    plt.plot([0, 0], [-1, 3], color='k', linewidth=1)
    plt.plot([-1, 3], [0, 0], color='k', linewidth=1)
    plt.plot(1, 0.4, color='k', marker='+')
#    plt.show()

def plot_data(x_prev, x, y_prev, y):
    plt.plot([x_prev, x], [y_prev, y], color='k', linestyle="dotted", marker='o')

def show():
    plt.show()

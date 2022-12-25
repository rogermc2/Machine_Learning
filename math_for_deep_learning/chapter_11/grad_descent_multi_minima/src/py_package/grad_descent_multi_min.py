import numpy as np
import matplotlib.pyplot as plt

def show_contours(x, y, z):
    plt.contourf(x, y, z, 10, cmap="Greys")
    # k means black
    plt.contour(x, y, z, colors='k', linewidths=1)
    plt.plot([0, 0], [-2, 2], color='k', linewidth=1)
    plt.plot([-2, 2], [0, 0], color='k', linewidth=1)

def plot_data(x_prev, x, y_prev, y, marker):
    plt.plot([x_prev, x], [y_prev, y], color='k', linestyle="dotted", marker=marker)

def show():
    plt.show()

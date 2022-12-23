import numpy as np
import matplotlib.pyplot as plt

def show_contours(xy, z):
    listmap = list (xy[0])
    contour_array = np.asarray(xy[0])
    num_rows = int (len(contour_array) / 2)
    contour_array = bm_array.reshape(num_rows, Row_Size, 3)
    plt.contourf(xy, z, 10, cmap="Greys")
    plt.show()

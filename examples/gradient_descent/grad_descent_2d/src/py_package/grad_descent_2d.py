import numpy as np
import matplotlib.pyplot as plt

def show_contours(x, y, z):
#    listmap = list (xy[0])
#    contour_array = np.asarray(xy[0])
#    num_rows = int (len(contour_array) / 2)
#    bm_array = bm_array.reshape(num_rows, Row_Size, 3)
    plt.contourf(x, y, z, 10, cmap="Greys")
    plt.show()

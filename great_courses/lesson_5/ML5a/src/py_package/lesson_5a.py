import numpy as np
import matplotlib.pyplot as plt

def show_bitmap(bitmap, Row_Size):
    bm_array = np.asarray(bitmap)
    num_rows = int (len(bitmap) / Row_Size)
    bm_array = bm_array.reshape(num_rows, Row_Size, 3)
    plt.imshow(bm_array)
    plt.show()

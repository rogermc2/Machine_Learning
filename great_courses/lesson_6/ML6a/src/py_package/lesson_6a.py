import numpy as np
import matplotlib.pyplot as plt

def show_bitmap(bitmap, Row_Size):
    listmap = list (bitmap[0])
    bm_array = np.asarray(bitmap[0])
    num_rows = int (len(bm_array) / Row_Size)
    bm_array = bm_array.reshape(num_rows, Row_Size, 3)
    plt.imshow(bm_array)
    plt.show()

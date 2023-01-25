import numpy as np
import matplotlib.pyplot as plt

def show_bitmap(bitmap, Row_Size):
    print ("show_bitmap")
    listmap = list (bitmap[0])
    print ("show_bitmap listmap set")
    bm_array = np.asarray(bitmap[0])
    print ("show_bitmap bm_array set")
    num_rows = int (len(bm_array) / Row_Size)
    bm_array = bm_array.reshape(num_rows, Row_Size, 3)
    plt.imshow(bm_array)
    plt.show()

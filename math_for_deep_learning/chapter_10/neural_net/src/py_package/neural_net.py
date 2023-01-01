import numpy as np
import matplotlib.pyplot as plt

def show_image (data):
    img = np.array(data).reshape(14,14)
    plt.imshow(img, cmap='gray', vmin=0, vmax=255)
    plt.show()



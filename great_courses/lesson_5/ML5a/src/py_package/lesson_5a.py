import numpy as np
from keras.preprocessing import image

def display (file_name):
    img = image.load_img(file_name)
    display(img)

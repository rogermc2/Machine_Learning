#!/usr/bin/env python3.9
import numpy as np
#import tensorflow
from keras.preprocessing import image

def display (file_name):
    print("display")
    img = image.load_img(file_name)
    display(img)
    print("display done")

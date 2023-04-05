import numpy as np
from matplotlib import pyplot as plt

def plot(data1, data2):
    print("plot data1 x points", data1[11][0], data1[12][0], data1[13][0], data1[14][0], data1[15][0], data2[16][0], data2[17][0], data2[18][0], data2[19][0], data1[20][0])
    print("plot data1 y points", data1[11][1], data1[12][1], data1[13][1], data1[14][1], data1[15][1], data2[16][1], data2[17][1], data2[18][1], data2[19][1], data1[20][1])
    print("plot data2 x points", data2[11][0], data2[12][0], data2[13][0], data2[14][0], data1[15][0], data2[16][0], data2[17][0], data2[18][0], data2[19][0], data1[20][0])
    print("plot data2 y points", data2[11][1], data2[12][1], data2[13][1], data2[14][1], data1[15][1], data2[16][1], data2[17][1], data2[18][1], data2[19][1], data1[20][1])
    plt.plot(data1[:][0], data1[:][1],'o',color='r')
    plt.plot(data2[:][0], data2[:][1],'o',color='b')
    plt.xticks(np.arange(0, np.max(data1) + 20, 20.0))
    plt.yticks(np.arange(0, np.max(data1) + 20, 20.0))
    plt.show()

from matplotlib import pyplot as plt

def plot(data1, data2):
    plt.plot(data1[0][209], data1[0][210],'o',color='r')
    plt.plot(data2[0][209], data2[0][210],'o',color='b')
    plt.show()

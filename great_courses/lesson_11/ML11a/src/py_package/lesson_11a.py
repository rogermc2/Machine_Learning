from matplotlib import pyplot as plt

def plot(data1, data2):
    plt.plot(data1[0][0], data1[0][1],'o',color='r')
    plt.plot(data2[0][0], data2[0][1],'o',color='b')
    plt.show()

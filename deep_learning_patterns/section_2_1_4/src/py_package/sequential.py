
import matplotlib
from matplotlib import pyplot

def plot(data1, data2):
    dp1 = np.array (data1)
    dp2 = np.array (data2)
    plt.plot(dp1[:,0], dp1[:,1],'o',color='r')
    plt.plot(dp2[:,0], dp2[:,1],'o',color='b')
    plt.xticks(np.arange(0, np.max(data1) + 20, 20.0))
    plt.yticks(np.arange(0, np.max(data1) + 20, 20.0))
    plt.show()
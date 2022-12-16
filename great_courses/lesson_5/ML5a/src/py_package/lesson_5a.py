import matplotlib.pyplot as plt

def show(bitmap):
    print("bitmap size:", len(bitmap), "x", len(bitmap[0]))
    print(bitmap[10][0:15])
    plt.imshow(bitmap)
    plt.show()

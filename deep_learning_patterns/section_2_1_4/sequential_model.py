from tensorflow.keras import Input
from tensorflow.keras import Sequential
from tensorflow.keras.layers import Dense

# print (Input (shape=(13,)))

model = Sequential()
model.add (Dense (10, input_shape=(13,)))
model.add (Dense (10))
model.add (Dense (1))


import os
import sys
import sikit-learn

sys.path.insert(0, '..')

cwd = os.getcwd()
# print ('load.py  cwd: ', cwd)
# print (os.listdir(os.getcwd()))

with open("../diabetes.csv", "r") as f:
    data = f.readlines()

feats = data[0]
feats = feats.replace('\n', '')
feats = feats.split(",")

print ("Features:")
print (feats)

feats = feats[0:(len(feats)-1)]
dat = []
labels = []
for i in range(1, len(data)):
    line = data[i]
    line = line.replace('\n', '')
    csvline = line.split(",")
    labels = labels + [int(csvline[len(csvline)-1])]
    csvline = [float(csvline[i]) for i in range(len(csvline) - 1)]
    dat = dat + [csvline]

print ("Number of samples: ", len(dat))
print ("Samples row 15: ", dat[15])

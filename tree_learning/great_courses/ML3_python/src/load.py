import os
import sys
sys.path.insert(0, '..')
import Tree2
print ('Tree2 imported ')
from Tree2 import base
print ('base imported ')
from Tree2 import classes
print ('classes imported ')

cwd = os.getcwd()
print ('load cwd: ', cwd)

print (os.listdir(os.getcwd()))

cwd = os.path.join(cwd, 'Tree2')
os.chdir(cwd)
print (os.listdir(os.getcwd()))

import os
import Tree2
from Tree2 import base
from Tree2 import classes

cwd = os.getcwd()
print ('load cwd: ', cwd)

print (os.listdir(os.getcwd()))

cwd = os.path.join(cwd, 'Tree2')
os.chdir(cwd)
print (os.listdir(os.getcwd()))

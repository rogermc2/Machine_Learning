import os
import sys
sys.path.insert(0, '..')
import Tree2
from Tree2 import base
from Tree2 import classes

cwd = os.getcwd()
#print ('load.py  cwd: ', cwd)

#print (os.listdir(os.getcwd()))

cwd = os.path.join(cwd, '../../Tree2')
os.chdir(cwd)

#import sys
#import subprocess

# implement pip as a subprocess:
#def install(package):
#    subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'dowhy'])

import dowhy
from dowhy import CausalModel

#from sklearn import tree
#from sklearn.naive_bayes import MultinomialNB
#from sklearn.metrics import confusion_matrix
#
#import matplotlib
#from matplotlib import pyplot

def init_model(data):
    return CausalModel(data = data, treatment='treatment', outcome='y_factual', common_causes=xs.split('+'))

import numpy as np
import pandas as pd
import dowhy
from dowhy import CausalModel

#from sklearn import tree
#from sklearn.naive_bayes import MultinomialNB
#from sklearn.metrics import confusion_matrix
#
#import matplotlib
#from matplotlib import pyplot

def init_model(col, data_in, xs):
    print ("init_model data_in", len (data_in))
    print ("init_model col", col)
    data = pd.DataFrame(list(data_in))
    print("data", data.shape)
    data.columns = col
    return CausalModel(data = data_array, treatment='treatment', outcome='y_factual', common_causes=xs.split('+'))

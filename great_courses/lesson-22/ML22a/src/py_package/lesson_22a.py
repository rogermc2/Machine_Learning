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
    data = pd.DataFrame(list(data_in))
    data.columns = col
    return CausalModel(data = data, treatment='treatment', outcome='y_factual', common_causes=xs.split('+'))

def identify_effect(model):
    print ("identified_estimand: ", model.identify_effect())

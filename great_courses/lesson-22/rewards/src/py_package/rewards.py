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

def init_data(num_users, num_months, signup_months):
    df = pd.DataFrame({'user_id': np.repeat(np.arange(num_users), num_months),
                        # signup month == 0 means customer did not sign up
                      'signup_month': np.repeat(signup_months, num_months),
                      'month': np.tile(np.arange(1, num_months+1), num_users),
                      'spend': np.random.poisson(500, num_users*num_months)}) #np.random.beta(a=2, b=5, size=num_users * num_months)*1000 # centered at 500})
    # A customer is in the treatment group if and only if they are signed up
    df["treatment"] = df["signup_month"]>0
    # Simulating an effect of month (monotonically decreasing--customers buy less later in the year)
    df["spend"] = df["spend"] - df["month"]*10
    after_signup = (df["signup_month"] < df["month"]) & (df["treatment"])
    df.loc[after_signup,"spend"] = df[after_signup]["spend"] + 100
    print (df)
    return df
                      
                        
#def init_model(col, data_in, xs):
#    data = pd.DataFrame(list(data_in))
#    data.columns = col
#    return CausalModel(data = data, treatment='treatment', outcome='y_factual', common_causes=xs.split('+'))
#
#def identify_effect(model):
#    print ("identified_estimand: ", model.identify_effect())

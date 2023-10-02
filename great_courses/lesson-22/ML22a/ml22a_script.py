
import os, sys
sys.path.append(os.path.abspath("../../"))
import dowhy
from dowhy import CausalModel
import pandas as pd
import numpy as np

data= pd.read_csv("https://raw.githubusercontent.com/AMLab-Amsterdam/CEVAE/master/datasets/IHDP/csv/ihdp_npci_1.csv", header = None)

col =  ["treatment", "y_factual", "y_cfactual", "mu0", "mu1" ,]
 
for i in range(1,26):
    col.append("x"+str(i))
data.columns = col
data = data.astype({"treatment": bool})
print(data.head())
xs = ""
for i in range(1,26):
    xs += ("x"+str(i)+"+")
 
model=CausalModel(data = data,
                  treatment='treatment',
                  outcome='y_factual',
                  common_causes=xs.split('+'))

#Identify the causal effect
print("Identifying estimand takes many minutes")
identified_estimand = model.identify_effect()
print("identified_estimand: ", identified_estimand)
print("done")
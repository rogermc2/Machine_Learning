import pandas as pd
import numpy as np
num_users = 10000
num_months = 12


# signup_months == 0 means customer did not sign up
signup_months = np.random.choice(np.arange(1, num_months), num_users) * np.random.randint(0,2, size=num_users)
print("signup_months: ", len(signup_months))

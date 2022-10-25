import numpy as np

from sklearn.datasets import load_digits
from sklearn.exceptions import ConvergenceWarning
from sklearn.neural_network import MLPClassifier

from sklearn.preprocessing import MinMaxScaler
from sklearn.utils._testing import ignore_warnings

X_digits, y_digits = load_digits(n_class=2, return_X_y=True)

X_digits_binary = MinMaxScaler().fit_transform(X_digits[:200])
y_digits_binary = y_digits[:200]

X = X_digits_binary[:100]
y = y_digits_binary[:100]

alpha_vectors = []
alpha_values = np.arange(2)
print ("alpha_values", alpha_values)
absolute_sum = lambda x: np.sum(np.abs(x))

for alpha in alpha_values:
    mlp = MLPClassifier(hidden_layer_sizes=10, alpha=alpha, random_state=1)
    with ignore_warnings(category=ConvergenceWarning):
        mlp.fit(X, y)
    print (absolute_sum(mlp.coefs_[0]))
    print (absolute_sum(mlp.coefs_[1]))
    alpha_vectors.append(
        np.array([absolute_sum(mlp.coefs_[0]), absolute_sum(mlp.coefs_[1])]))

for i in range(len(alpha_values)):
	print (alpha_vectors[i])

for i in range(len(alpha_values) - 1):
    assert (alpha_vectors[i] > alpha_vectors[i + 1]).all()


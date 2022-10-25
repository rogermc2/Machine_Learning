
import pytest
import sys
import warnings
import re

import numpy as np
import joblib

from numpy.testing import (
    assert_almost_equal,
    assert_array_equal,
    assert_allclose,
)

from sklearn.neural_network import MLPClassifier
from sklearn.utils._testing import ignore_warnings


X = np.array([[0.6, 0.8, 0.7]])
y = np.array([0])
mlp = MLPClassifier(
    solver="sgd",
    learning_rate_init=0.1,
    alpha=0.1,
    activation="logistic",
    random_state=1,
    max_iter=1,
    hidden_layer_sizes=2,
    momentum=0,
)

mlp.coefs_ = [0] * 2
mlp.intercepts_ = [0] * 2
mlp.n_outputs_ = 1
mlp.coefs_[0] = np.array([[0.1, 0.2], [0.3, 0.1], [0.5, 0]])
mlp.coefs_[1] = np.array([[0.1], [0.2]])
mlp.intercepts_[0] = np.array([0.1, 0.1])
mlp.intercepts_[1] = np.array([1.0])
mlp._coef_grads = [] * 2
mlp._intercept_grads = [] * 2
mlp.n_features_in_ = 3

# Initialize parameters
mlp.n_iter_ = 0
mlp.learning_rate_ = 0.1

# Compute the number of layers
mlp.n_layers_ = 3

# Pre-allocate gradient matrices
mlp._coef_grads = [0] * (mlp.n_layers_ - 1)
mlp._intercept_grads = [0] * (mlp.n_layers_ - 1)

mlp.out_activation_ = "logistic"
mlp.t_ = 0
mlp.best_loss_ = np.inf
mlp.loss_curve_ = []
mlp._no_improvement_count = 0
mlp._intercept_velocity = [
np.zeros_like(intercepts) for intercepts in mlp.intercepts_
]
mlp._coef_velocity = [np.zeros_like(coefs) for coefs in mlp.coefs_]

mlp.partial_fit(X, y, classes=[0, 1])

assert_almost_equal(
    mlp.coefs_[0],
    np.array([[0.098, 0.195756], [0.2956664, 0.096008], [0.4939998, -0.002244]]),
    decimal=3,
)
assert_almost_equal(mlp.coefs_[1], np.array([[0.04706], [0.154089]]), decimal=3)
assert_almost_equal(mlp.intercepts_[0], np.array([0.098333, 0.09626]), decimal=3)
assert_almost_equal(mlp.intercepts_[1], np.array(0.9235), decimal=3)

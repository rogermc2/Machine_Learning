
import pytest
import sys
import warnings
import re

import numpy as np

from numpy.testing import (
    assert_almost_equal,
    assert_array_equal,
    assert_allclose,
)

from sklearn.datasets import make_regression, make_multilabel_classification
from sklearn.exceptions import ConvergenceWarning
from io import StringIO
from sklearn.metrics import roc_auc_score
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import LabelBinarizer
from sklearn.preprocessing import MinMaxScaler, scale
from scipy.sparse import csr_matrix
from sklearn.utils._testing import ignore_warnings

ACTIVATION_TYPES = ["identity", "logistic", "tanh", "relu"]

for n_labels in [2, 2]:
    n_samples = 5
    n_features = 4
    random_state = np.random.RandomState(seed=42)
    X = random_state.rand(n_samples, n_features)
    y = 1 + np.mod(np.arange(n_samples) + 1, n_labels)
    Y = LabelBinarizer().fit_transform(y)
    # print ("test X", X)
    # print ("test y", y)
    # print ("test indices Y", Y)

    for activation in ACTIVATION_TYPES:
         print ("activation", activation)
         mlp = MLPClassifier(
            activation=activation,
            hidden_layer_sizes=10,
            solver="lbfgs",
            alpha=1e-5,
            learning_rate_init=0.2,
            max_iter=1,
            random_state=1,
        )
         mlp.fit (X, y)
         print ("L50 mlp.intercepts_", mlp.intercepts_)
         theta = np.hstack([l.ravel() for l in mlp.coefs_ + mlp.intercepts_])
        
         layer_units = [X.shape[1]] + [mlp.hidden_layer_sizes] + [mlp.n_outputs_]

         activations = []
         deltas = []
         coef_grads = []
         intercept_grads = []

         activations.append(X)
         for i in range(mlp.n_layers_ - 1):
            activations.append(np.empty((X.shape[0], layer_units[i + 1])))
            deltas.append(np.empty((X.shape[0], layer_units[i + 1])))

            fan_in = layer_units[i]
            fan_out = layer_units[i + 1]
            coef_grads.append(np.empty((fan_in, fan_out)))
            intercept_grads.append(np.empty(fan_out))

         # analytically compute the gradients
         def loss_grad_fun(t):
            return mlp._loss_grad_lbfgs(
                t, X, Y, activations, deltas, coef_grads, intercept_grads
            )

         [value, grad] = loss_grad_fun(theta)
         numgrad = np.zeros(np.size(theta))
         n = np.size(theta, 0)
         E = np.eye(n)
         epsilon = 1e-5
         print ("L80 theta length n:", n);
         print ("L80 mlp.intercepts_", mlp.intercepts_)
         # numerically compute the gradients
         for i in range(n):
            dtheta = E[:, i] * epsilon
            numgrad[i] = (
                loss_grad_fun(theta + dtheta)[0] - loss_grad_fun(theta - dtheta)[0]
            ) / (epsilon * 2.0)
         # print ("numgrad", numgrad)
         # print ("grad", grad)
         # assert_almost_equal(numgrad, grad)

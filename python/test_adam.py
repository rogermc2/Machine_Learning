
import numpy as np

from numpy.testing import (
    assert_almost_equal,
    assert_array_equal,
    assert_allclose,
)
from sklearn.datasets import load_digits, load_iris
import numpy as np

from sklearn.neural_network._stochastic_optimizers import (
    BaseOptimizer,
    SGDOptimizer,
    AdamOptimizer,
)
from sklearn.utils._testing import assert_array_equal

shapes = [(4, 6), (6, 8), (7, 8, 9)]

params = [np.zeros(shape) for shape in shapes]
lr = 0.001
epsilon = 1e-8
rng = np.random.RandomState(0)
print ("testing adam optimizer")

for beta_1 in np.arange(0.9, 1.0, 0.05):
    for beta_2 in np.arange(0.995, 1.0, 0.001):
        optimizer = AdamOptimizer(params, lr, beta_1, beta_2, epsilon)
        ms = [rng.random_sample(shape) for shape in shapes]
        vs = [rng.random_sample(shape) for shape in shapes]
        t = 10
        optimizer.ms = ms
        optimizer.vs = vs
        optimizer.t = t - 1
        grads = [rng.random_sample(shape) for shape in shapes]

        ms = [beta_1 * m + (1 - beta_1) * grad for m, grad in zip(ms, grads)]
        vs = [beta_2 * v + (1 - beta_2) * (grad**2) for v, grad in zip(vs, grads)]
        learning_rate = lr * np.sqrt(1 - beta_2**t) / (1 - beta_1**t)
        updates = [-learning_rate * m / (np.sqrt(v) + epsilon) for m, v in zip(ms, vs)]

        expected = [param + update for param, update in zip(params, updates)]
        optimizer.update_params(params, grads)

        # for exp, param in zip(expected, params):
        #   assert_array_equal(exp, param)

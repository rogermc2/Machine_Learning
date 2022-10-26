

import numpy as np

from numpy.testing import (
    assert_almost_equal,
    assert_array_equal,
    assert_allclose,
)
from sklearn.datasets import make_classification
from sklearn.neural_network import MLPClassifier

X, y = make_classification(n_samples=50, n_features=5, random_state=0)

for shuffle in [True, False]:
    mlp1 = MLPClassifier(
        hidden_layer_sizes=1,
        max_iter=1,
        batch_size=1,
        random_state=0,
        shuffle=shuffle,
    )
    mlp2 = MLPClassifier(
        hidden_layer_sizes=1,
        max_iter=1,
        batch_size=1,
        random_state=0,
        shuffle=shuffle,
    )
    mlp1.fit(X, y)
    mlp2.fit(X, y)

    assert np.array_equal(mlp1.coefs_[0], mlp2.coefs_[0])
    print ("shuffle test passed")

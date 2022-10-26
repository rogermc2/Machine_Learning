from sklearn.datasets import make_multilabel_classification
from sklearn.neural_network import MLPClassifier

X, y = make_multilabel_classification(
        n_samples=50, random_state=0, return_indicator=True)

# test partial fit method
mlp = MLPClassifier(
        solver="sgd",
        hidden_layer_sizes=50,
        max_iter=150,
        random_state=0,
        activation="logistic",
        alpha=1e-5,
        learning_rate_init=0.2,)

for i in range(100):
        mlp.partial_fit(X, y, classes=[0, 1, 2, 3, 4])
assert mlp.score(X, y) > 0.9
print ("mlp test passed")
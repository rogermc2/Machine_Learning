
from abc import ABCMeta
from base import BaseEstimator
from base import MultiOutputMixin
from base import ClassifierMixin
__all__ = [
           "BaseDecisionTree",
           "DecisionTreeClassifier",
           "DecisionTreeRegressor",
           "ExtraTreeClassifier",
           "ExtraTreeRegressor",
           "export_graphviz",
           "plot_tree",
           "export_text",
           ]

class BaseDecisionTree (MultiOutputMixin, BaseEstimator, metaclass=ABCMeta):
        # Base class for decision trees.#
        #    Warning: This class should not be used directly.
        #    Use derived classes instead.
        
#    @abstractmethod
    def __init__(
                 self,
                 *,
                 criterion,
                 splitter,
                 max_depth,
                 min_samples_split,
                 min_samples_leaf,
                 min_weight_fraction_leaf,
                 max_features,
                 max_leaf_nodes,
                 random_state,
                 min_impurity_decrease,
                 class_weight=None,
                 ccp_alpha=0.0,
                 ):
        self.criterion = criterion
        self.splitter = splitter
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.min_samples_leaf = min_samples_leaf
        self.min_weight_fraction_leaf = min_weight_fraction_leaf
        self.max_features = max_features
        self.max_leaf_nodes = max_leaf_nodes
        self.random_state = random_state
        self.min_impurity_decrease = min_impurity_decrease
        self.class_weight = class_weight
        self.ccp_alpha = ccp_alpha

    def fit(self, X, y, sample_weight=None, check_input=True):
        random_state = check_random_state(self.random_state)

        check_scalar(self.ccp_alpha,
                         name="ccp_alpha",
                         target_type=numbers.Real,
                         min_val=0.0,)
        if check_input:
            # Need to validate separately here.
            # We can't pass multi_ouput=True because that would allow y to be
            # csr.
            check_X_params = dict(dtype=DTYPE, accept_sparse="csc")
            check_y_params = dict(ensure_2d=False, dtype=None)
            X, y = self._validate_data(X, y, validate_separately=(check_X_params, check_y_params))
        
        if issparse(X):
            X.sort_indices()
        
        if X.indices.dtype != np.intc or X.indptr.dtype != np.intc:
            raise ValueError("No support for np.int64 index based sparse matrices")
                
        if self.criterion == "poisson":
            if np.any(y < 0):
                raise ValueError("Some value(s) of y are negative which is"
                                         " not allowed for Poisson regression.")
        if np.sum(y) <= 0:
                raise ValueError( "Sum of y is not positive which is "
                                         "necessary for Poisson regression.")
    # Determine output settings
        n_samples, self.n_features_in_ = X.shape
        is_classification = is_classifier(self)

        y = np.atleast_1d(y)
        expanded_class_weight = None

        if y.ndim == 1:
            # reshape is necessary to preserve the data contiguity against vs
            # [:, np.newaxis] that does not.
            y = np.reshape(y, (-1, 1))

            self.n_outputs_ = y.shape[1]

            if is_classification:
                check_classification_targets(y)
                y = np.copy(y)

                self.classes_ = []
                self.n_classes_ = []

                if self.class_weight is not None:
                    y_original = np.copy(y)

                y_encoded = np.zeros(y.shape, dtype=int)
                for k in range(self.n_outputs_):
                    classes_k, y_encoded[:, k] = np.unique(y[:, k], return_inverse=True)
                    self.classes_.append(classes_k)
                    self.n_classes_.append(classes_k.shape[0])
                y = y_encoded

                if self.class_weight is not None:
                    expanded_class_weight = compute_sample_weight(self.class_weight, y_original)

                self.n_classes_ = np.array(self.n_classes_, dtype=np.intp)

            if getattr(y, "dtype", None) != DOUBLE or not y.flags.contiguous:
                y = np.ascontiguousarray(y, dtype=DOUBLE)

            # Check parameters
            if self.max_depth is not None:
                check_scalar(self.max_depth,
                             name="max_depth",
                             target_type=numbers.Integral,
                             min_val=1,)
            max_depth = np.iinfo(np.int32).max if self.max_depth is None else self.max_depth

            if isinstance(self.min_samples_leaf, numbers.Integral):
                check_scalar(
                             self.min_samples_leaf,
                             name="min_samples_leaf",
                             target_type=numbers.Integral,
                             min_val=1,
                             )
                min_samples_leaf = self.min_samples_leaf
            else:  # float
                check_scalar(
                             self.min_samples_leaf,
                             name="min_samples_leaf",
                             target_type=numbers.Real,
                             min_val=0.0,
                             include_boundaries="neither",
                             )
                min_samples_leaf = int(ceil(self.min_samples_leaf * n_samples))

            if isinstance(self.min_samples_split, numbers.Integral):
                check_scalar(
                     self.min_samples_split,
                     name="min_samples_split",
                     target_type=numbers.Integral,
                     min_val=2,
                 )
                min_samples_split = self.min_samples_split
            else:  # float
                check_scalar(
                             self.min_samples_split,
                             name="min_samples_split",
                             target_type=numbers.Real,
                             min_val=0.0,
                             max_val=1.0,
                             include_boundaries="right",
                             )
                min_samples_split = int(ceil(self.min_samples_split * n_samples))
                min_samples_split = max(2, min_samples_split)

            min_samples_split = max(min_samples_split, 2 * min_samples_leaf)

            check_scalar(
                         self.min_weight_fraction_leaf,
                         name="min_weight_fraction_leaf",
                         target_type=numbers.Real,
                         min_val=0.0,
                         max_val=0.5,
                         )

            if isinstance(self.max_features, str):
                if self.max_features == "auto":
                    if is_classification:
                        max_features = max(1, int(np.sqrt(self.n_features_in_)))
                    else:
                        max_features = self.n_features_in_
                elif self.max_features == "sqrt":
                    max_features = max(1, int(np.sqrt(self.n_features_in_)))
                elif self.max_features == "log2":
                    max_features = max(1, int(np.log2(self.n_features_in_)))
                else:
                    raise ValueError(
                                     "Invalid value for max_features. "
                                     "Allowed string values are 'auto', "
                                     "'sqrt' or 'log2'."
                                     )
            elif self.max_features is None:
                max_features = self.n_features_in_
            elif isinstance(self.max_features, numbers.Integral):
                check_scalar(
                             self.max_features,
                             name="max_features",
                             target_type=numbers.Integral,
                             min_val=1,
                             include_boundaries="left",
                             )
                max_features = self.max_features
            else:  # float
                check_scalar(
                             self.max_features,
                             name="max_features",
                             target_type=numbers.Real,
                             min_val=0.0,
                             max_val=1.0,
                             include_boundaries="right",
                             )
                if self.max_features > 0.0:
                    max_features = max(1, int(self.max_features * self.n_features_in_))
                else:
                    max_features = 0
                        
            self.max_features_ = max_features

            if self.max_leaf_nodes is not None:
                check_scalar(
                             self.max_leaf_nodes,
                             name="max_leaf_nodes",
                             target_type=numbers.Integral,
                             min_val=2,
                             )
            max_leaf_nodes = -1 if self.max_leaf_nodes is None else self.max_leaf_nodes

        check_scalar(
                     self.min_impurity_decrease,
                     name="min_impurity_decrease",
                     target_type=numbers.Real,
                     min_val=0.0,
                     )

        if len(y) != n_samples:
            raise ValueError ("Number of labels=%d does not match number of samples=%d"
                              % (len(y), n_samples)
                                                                                                                 )
        if sample_weight is not None:
            sample_weight = _check_sample_weight(sample_weight, X, DOUBLE)

        if expanded_class_weight is not None:
            if sample_weight is not None:
                sample_weight = sample_weight * expanded_class_weight
            else:
                sample_weight = expanded_class_weight

            # Set min_weight_leaf from min_weight_fraction_leaf
            if sample_weight is None:
                min_weight_leaf = self.min_weight_fraction_leaf * n_samples
            else:
                min_weight_leaf = self.min_weight_fraction_leaf * np.sum(sample_weight)

        # Build tree
        criterion = self.criterion
        if not isinstance(criterion, Criterion):
            if is_classification:
                criterion = CRITERIA_CLF[self.criterion](
                                                         self.n_outputs_, self.n_classes_
                                                         )
        else:
            criterion = CRITERIA_REG[self.criterion](self.n_outputs_, n_samples)
        # TODO: Remove in v1.2
        if self.criterion == "mse":
            warnings.warn(
                          "Criterion 'mse' was deprecated in v1.0 and will be "
                          "removed in version 1.2. Use `criterion='squared_error'` "
                          "which is equivalent.",
                          FutureWarning,
                          )
        elif self.criterion == "mae":
            warnings.warn(
                          "Criterion 'mae' was deprecated in v1.0 and will be "
                          "removed in version 1.2. Use `criterion='absolute_error'` "
                          "which is equivalent.",
                          FutureWarning,
                          )
        else:
            # Make a deepcopy in case the criterion has mutable attributes that
            # might be shared and modified concurrently during parallel fitting
            criterion = copy.deepcopy(criterion)

        SPLITTERS = SPARSE_SPLITTERS if issparse(X) else DENSE_SPLITTERS

        splitter = self.splitter
        if not isinstance(self.splitter, Splitter):
            splitter = SPLITTERS[self.splitter](
                                                criterion,
                                                self.max_features_,
                                                min_samples_leaf,
                                                min_weight_leaf,
                                                random_state,
                                                )

        if is_classifier(self):
            self.tree_ = Tree(self.n_features_in_, self.n_classes_, self.n_outputs_)
        else:
            self.tree_ = Tree(
                              self.n_features_in_,
                              # TODO: tree shouldn't need this in this case
                              np.array([1] * self.n_outputs_, dtype=np.intp),
                              self.n_outputs_,
                              )

        # Use BestFirst if max_leaf_nodes given; use DepthFirst otherwise
        if max_leaf_nodes < 0:
            builder = DepthFirstTreeBuilder(
                                            splitter,
                                            min_samples_split,
                                            min_samples_leaf,
                                            min_weight_leaf,
                                            max_depth,
                                            self.min_impurity_decrease,
                                            )
        else:
            builder = BestFirstTreeBuilder(
                                           splitter,
                                           min_samples_split,
                                           min_samples_leaf,
                                           min_weight_leaf,
                                           max_depth,
                                           max_leaf_nodes,
                                           self.min_impurity_decrease,
                                           )

        builder.build(self.tree_, X, y, sample_weight)

        if self.n_outputs_ == 1 and is_classifier(self):
            self.n_classes_ = self.n_classes_[0]
            self.classes_ = self.classes_[0]

        self._prune_tree()

        return self

class DecisionTreeClassifier (ClassifierMixin, BaseDecisionTree):
    def __init__(
                 self,
                 *,
                 criterion="gini",
                 splitter="best",
                 max_depth=None,
                 min_samples_split=2,
                 min_samples_leaf=1,
                 min_weight_fraction_leaf=0.0,
                 max_features=None,
                 random_state=None,
                 max_leaf_nodes=None,
                 min_impurity_decrease=0.0,
                 class_weight=None,
                 ccp_alpha=0.0,
                 ):
        super().__init__(
                         criterion=criterion,
                         splitter=splitter,
                         max_depth=max_depth,
                         min_samples_split=min_samples_split,
                         min_samples_leaf=min_samples_leaf,
                         min_weight_fraction_leaf=min_weight_fraction_leaf,
                         max_features=max_features,
                         max_leaf_nodes=max_leaf_nodes,
                         class_weight=class_weight,
                         random_state=random_state,
                         min_impurity_decrease=min_impurity_decrease,
                         ccp_alpha=ccp_alpha,
                         )
    
    def fit(self, X, y, sample_weight=None, check_input=True):
        # Build a decision tree classifier from the training set (X, y).
        super().fit(
            X,
            y,
            sample_weight=sample_weight,
            check_input=check_input,
        )
        return self


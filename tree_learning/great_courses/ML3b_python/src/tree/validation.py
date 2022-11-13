""" From /Applications_Packages/scikit-learn/sklearn/utils/validation.py"""

import sys
import operator
import numpy as np
import scipy.sparse as sp


def check_array(
    array,
    accept_sparse=False,
    *,
    accept_large_sparse=True,
    dtype="numeric",
    order=None,
    copy=False,
    force_all_finite=True,
    ensure_2d=True,
    allow_nd=False,
    ensure_min_samples=1,
    ensure_min_features=1,
    estimator=None,
    input_name="",):

    """Input validation on an array, list, sparse matrix or similar.

    By default, the input is checked to be a non-empty 2D array containing
    only finite values. If the dtype of the array is object, attempt
    converting to float, raising on failure.

    Returns
    -------
    array_converted : object
        The converted and validated array.
    """
    if isinstance(array, np.matrix):
        warnings.warn(
            "np.matrix usage is deprecated in 1.0 and will raise a TypeError "
            "in 1.2. Please convert to a numpy array with np.asarray. For "
            "more information see: "
            "https://numpy.org/doc/stable/reference/generated/numpy.matrix.html",  # noqa
            FutureWarning,)

    # store reference to original array to check if copy is needed when
    # function returns
    array_orig = array

    # store whether originally we wanted numeric dtype
    dtype_numeric = isinstance(dtype, str) and dtype == "numeric"

    dtype_orig = getattr(array, "dtype", None)
    if not hasattr(dtype_orig, "kind"):
        # not a data type (e.g. a column named dtype in a pandas DataFrame)
        dtype_orig = None

    # check if the object contains several dtypes (typically a pandas
    # DataFrame), and store them. If not, store None.
    dtypes_orig = None
    pandas_requires_conversion = False
    if hasattr(array, "dtypes") and hasattr(array.dtypes, "__array__"):
        # throw warning if columns are sparse. If all columns are sparse, then
        # array.sparse exists and sparsity will be preserved (later).
        with suppress(ImportError):
            from pandas.api.types import is_sparse

            if not hasattr(array, "sparse") and array.dtypes.apply(is_sparse).any():
                warnings.warn(
                    "pandas.DataFrame with sparse columns found."
                    "It will be converted to a dense numpy array."
                )

        dtypes_orig = []
        for dtype_iter in array.dtypes:
            if dtype_iter.kind == "b":
                # pandas boolean dtype __array__ interface coerces bools to objects
                dtype_iter = np.dtype(object)
            elif _pandas_dtype_needs_early_conversion(dtype_iter):
                pandas_requires_conversion = True

            dtypes_orig.append(dtype_iter)

        if all(isinstance(dtype_iter, np.dtype) for dtype_iter in dtypes_orig):
            dtype_orig = np.result_type(*dtypes_orig)

    if dtype_numeric:
        if dtype_orig is not None and dtype_orig.kind == "O":
            # if input is object, convert to float.
            dtype = np.float64
        else:
            dtype = None

    if isinstance(dtype, (list, tuple)):
        if dtype_orig is not None and dtype_orig in dtype:
            # no dtype conversion required
            dtype = None
        else:
            # dtype conversion required. Let's select the first element of the
            # list of accepted types.
            dtype = dtype[0]

    if pandas_requires_conversion:
        # pandas dataframe requires conversion earlier to handle extension dtypes with
        # nans
        array = array.astype(dtype)
        # Since we converted here, we do not need to convert again later
        dtype = None

    if force_all_finite not in (True, False, "allow-nan"):
        raise ValueError(
            'force_all_finite should be a bool or "allow-nan". Got {!r} instead'.format(
                force_all_finite
            )
        )

    estimator_name = _check_estimator_name(estimator)
    context = " by %s" % estimator_name if estimator is not None else ""

    # When all dataframe columns are sparse, convert to a sparse array
    if hasattr(array, "sparse") and array.ndim > 1:
        # DataFrame.sparse only supports `to_coo`
        array = array.sparse.to_coo()
        if array.dtype == np.dtype("object"):
            unique_dtypes = set([dt.subtype.name for dt in array_orig.dtypes])
            if len(unique_dtypes) > 1:
                raise ValueError(
                    "Pandas DataFrame with mixed sparse extension arrays "
                    "generated a sparse matrix with object dtype which "
                    "can not be converted to a scipy sparse matrix."
                    "Sparse extension arrays should all have the same "
                    "numeric type."
                )

    if sp.issparse(array):
        _ensure_no_complex_data(array)
        array = _ensure_sparse_format(
            array,
            accept_sparse=accept_sparse,
            dtype=dtype,
            copy=copy,
            force_all_finite=force_all_finite,
            accept_large_sparse=accept_large_sparse,
            estimator_name=estimator_name,
            input_name=input_name,
        )
    else:
        # If np.array(..) gives ComplexWarning, then we convert the warning
        # to an error. This is needed because specifying a non complex
        # dtype to the function converts complex to real dtype,
        # thereby passing the test made in the lines following the scope
        # of warnings context manager.
        with warnings.catch_warnings():
            try:
                warnings.simplefilter("error", ComplexWarning)
                if dtype is not None and np.dtype(dtype).kind in "iu":
                    # Conversion float -> int should not contain NaN or
                    # inf (numpy#14412). We cannot use casting='safe' because
                    # then conversion float -> int would be disallowed.
                    array = np.asarray(array, order=order)
                    if array.dtype.kind == "f":
                        _assert_all_finite(
                            array,
                            allow_nan=False,
                            msg_dtype=dtype,
                            estimator_name=estimator_name,
                            input_name=input_name,
                        )
                    array = array.astype(dtype, casting="unsafe", copy=False)
                else:
                    array = np.asarray(array, order=order, dtype=dtype)
            except ComplexWarning as complex_warning:
                raise ValueError(
                    "Complex data not supported\n{}\n".format(array)
                ) from complex_warning

        # It is possible that the np.array(..) gave no warning. This happens
        # when no dtype conversion happened, for example dtype = None. The
        # result is that np.array(..) produces an array of complex dtype
        # and we need to catch and raise exception for such cases.
        _ensure_no_complex_data(array)

        if ensure_2d:
            # If input is scalar raise error
            if array.ndim == 0:
                raise ValueError(
                    "Expected 2D array, got scalar array instead:\narray={}.\n"
                    "Reshape your data either using array.reshape(-1, 1) if "
                    "your data has a single feature or array.reshape(1, -1) "
                    "if it contains a single sample.".format(array)
                )
            # If input is 1D raise error
            if array.ndim == 1:
                raise ValueError(
                    "Expected 2D array, got 1D array instead:\narray={}.\n"
                    "Reshape your data either using array.reshape(-1, 1) if "
                    "your data has a single feature or array.reshape(1, -1) "
                    "if it contains a single sample.".format(array)
                )

        # make sure we actually converted to numeric:
        if dtype_numeric and array.dtype.kind in "OUSV":
            warnings.warn(
                "Arrays of bytes/strings is being converted to decimal "
                "numbers if dtype='numeric'. This behavior is deprecated in "
                "0.24 and will be removed in 1.1 (renaming of 0.26). Please "
                "convert your data to numeric values explicitly instead.",
                FutureWarning,
                stacklevel=2,
            )
            try:
                array = array.astype(np.float64)
            except ValueError as e:
                raise ValueError(
                    "Unable to convert array of bytes/strings "
                    "into decimal numbers with dtype='numeric'"
                ) from e
        if not allow_nd and array.ndim >= 3:
            raise ValueError(
                "Found array with dim %d. %s expected <= 2."
                % (array.ndim, estimator_name)
            )

        if force_all_finite:
            _assert_all_finite(
                array,
                input_name=input_name,
                estimator_name=estimator_name,
                allow_nan=force_all_finite == "allow-nan",
            )

    if ensure_min_samples > 0:
        n_samples = _num_samples(array)
        if n_samples < ensure_min_samples:
            raise ValueError(
                "Found array with %d sample(s) (shape=%s) while a"
                " minimum of %d is required%s."
                % (n_samples, array.shape, ensure_min_samples, context)
            )

    if ensure_min_features > 0 and array.ndim == 2:
        n_features = array.shape[1]
        if n_features < ensure_min_features:
            raise ValueError(
                "Found array with %d feature(s) (shape=%s) while"
                " a minimum of %d is required%s."
                % (n_features, array.shape, ensure_min_features, context)
            )

    if copy and np.may_share_memory(array, array_orig):
        array = np.array(array, dtype=dtype, order=order)

    return array


def _check_estimator_name(estimator):
    if estimator is not None:
        if isinstance(estimator, str):
            return estimator
        else:
            return estimator.__class__.__name__
    return None


def check_random_state(seed):
    """Turn seed into a np.random.RandomState instance
        
        Parameters
        ----------
        seed : None, int or instance of RandomState
        If seed is None, return the RandomState singleton used by np.random.
        If seed is an int, return a new RandomState instance seeded with seed.
        If seed is already a RandomState instance, return it.
        Otherwise raise ValueError.
        """
    if seed is None or seed is np.random:
        return np.random.mtrand._rand
    if isinstance(seed, numbers.Integral):
        return np.random.RandomState(seed)
    if isinstance(seed, np.random.RandomState):
        return seed
    raise ValueError("%r cannot be used to seed a numpy.random.RandomState",
                     " instance" % seed)

    
def check_scalar(x, name, target_type, *, min_val=None, max_val=None,
                 include_boundaries="both",):
    """Validate scalar parameters type and value.
        Returns
        -------
        x : numbers.Number   The validated number.
    """
    def type_name(t):
        """Convert type into humman readable string."""
        module = t.__module__
        qualname = t.__qualname__
        if module == "builtins":
            return qualname
        elif t == numbers.Real:
            return "float"
        elif t == numbers.Integral:
            return "int"
        return f"{module}.{qualname}"
    
    if not isinstance(x, target_type):
        if isinstance(target_type, tuple):
            types_str = ", ".join(type_name(t) for t in target_type)
            target_type_str = f"{{{types_str}}}"
        else:
            target_type_str = type_name(target_type)
    
        raise TypeError(
                        f"{name} must be an instance of {target_type_str}, not"
                        f" {type(x).__qualname__}.")

    expected_include_boundaries = ("left", "right", "both", "neither")
    if include_boundaries not in expected_include_boundaries:
        raise ValueError
        (f"Unknown value for `include_boundaries`:"
         " {repr(include_boundaries)}. "
         f"Possible values are: {expected_include_boundaries}.")
        
    if max_val is None and include_boundaries == "right":
        raise ValueError(
                            "`include_boundaries`='right' without specifying"
                            " explicitly `max_val` is inconsistent.")

    if min_val is None and include_boundaries == "left":
        raise ValueError("`include_boundaries`='left' without specifying"
                         " explicitly `min_val` is inconsistent.")
        
    comparison_operator = (operator.lt if include_boundaries in
                           ("left", "both") else operator.le)
    if min_val is not None and comparison_operator(x, min_val):
        raise ValueError
        (f"{name} == {x}, must be"
         f" {'>=' if include_boundaries in ('left', 'both') else '>'}\
          {min_val}.")

    comparison_operator = (operator.gt if include_boundaries in
                           ("right", "both") else operator.ge)
    if max_val is not None and comparison_operator(x, max_val):
        raise ValueError
        (f"{name} == {x}, must be"
         f" {'<=' if include_boundaries in ('right', 'both') else '<'}\
          {max_val}.")
    
    return x

        
def _get_feature_names(X):
    feature_names = None
    # extract feature names for support array containers
    if hasattr(X, "columns"):
        feature_names = np.asarray(X.columns, dtype=object)

    if feature_names is None or len(feature_names) == 0:
        return

    types = sorted(t.__qualname__ for t in set(type(v) for v in feature_names))
    # Warn when types are mixed and string is one of the types
    if len(types) > 1 and "str" in types:
        warnings.warn(
            "Feature names only support names that are all strings. "
            f"Got feature names with dtypes: {types}. An error will be raised "
            "in 1.2.",
            FutureWarning,)
        
        return

    # Only feature names of all strings are supported
    if len(types) == 1 and types[0] == "str":
        return feature_names

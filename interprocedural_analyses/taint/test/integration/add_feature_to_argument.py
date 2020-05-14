from builtins import __test_sink, __test_source


def add_feature_to_first(first, second):
    ...


def adds_and_taints():
    x = __test_source()
    add_feature_to_first(x, 0)
    return x


def propagate_add_feature(parameter):
    return add_feature_to_first(parameter, 0)


def add_via_value_of(first, second):
    ...


def test_add_via_value_of_second():
    x = __test_source()
    add_via_value_of(x, "second")
    return x


def test_add_feature_to_sink(parameter):
    add_feature_to_first(parameter, "")
    __test_sink(parameter)


def test_add_feature_in_comprehension():
    sources = [__test_source()]
    v = [s for s in sources if add_feature_to_first(s, 0)]
    __test_sink(v[0])


def test_add_feature_to_sink_in_comprehension(parameter):
    x = [s for s in [1, 2, 3] if add_feature_to_first(parameter, 0)]
    __test_sink(parameter)

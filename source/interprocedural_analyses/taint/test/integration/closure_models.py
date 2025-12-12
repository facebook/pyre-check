# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pysa import _test_sink, _test_source
from typing import Any


def inner_sink_wrapper():
    def inner_sink(x):
        return x

    inner_sink(_test_source())


class Reclassification:
    def __init__(self, inner, feature):
        self.inner = inner
        self.feature = feature

    def reclassify(self, feature):
        self.feature = feature
        return self.inner()


def reclassify(inner, feature) -> Reclassification:
    ...


def outer() -> Any:
    ...


def use_source(x):
    pass


def test() -> None:
    value = "benign"

    def return_taint():
        return _test_source()

    def return_model_taint():
        return value

    source = _test_source()

    def return_model_taint_tito():
        use_source(source)

    def return_model_query_tito():
        use_source(source)

    value = reclassify(
        inner=return_taint,
        feature="breadcrumb1",
    )

    value.reclassify(feature="breadcrumb2")

    value = reclassify(
        inner=return_model_taint,
        feature="breadcrumb1",
    )
    value.reclassify(feature="breadcrumb2")

    value = reclassify(
        inner=outer,
        feature="breadcrumb1",
    )
    value.reclassify(feature="breadcrumb2")

    value = reclassify(
        inner=return_model_taint_tito,
        feature="breadcrumb1",
    )
    value.reclassify(feature="breadcrumb2")

    value = reclassify(
        inner=return_model_query_tito,
        feature="breadcrumb1",
    )
    value.reclassify(feature="breadcrumb2")


def test_tito_transform():
    source = _test_source()

    def return_model_taint_tito():
        use_source(source)

    def return_model_query_tito():
        use_source(source)

    value = reclassify(
        inner=return_model_taint_tito,
        feature="breadcrumb1",
    )
    value.reclassify(feature="breadcrumb2")

    value = reclassify(
        inner=return_model_query_tito,
        feature="breadcrumb1",
    )
    value.reclassify(feature="breadcrumb2")


def some_decorator(func):
    def wrapper_func():
        func()

    return wrapper_func


def test_dsl_source(some_data: str) -> None:
    def local_function_capturing_local_variable():
        _test_sink(some_data)


def test_dsl_decorator_source(some_data: str) -> None:
    @some_decorator
    def decorated_local_function_capturing_local_variable():
        _test_sink(some_data)


def captured_variable_model_tito():
    complicated_name = _test_source()

    # model simulates captured variables returned
    def model_all_captured_as_tito():
        complicated_name

    _test_sink(model_all_captured_as_tito())


def captured_variable_model_parameter_source():
    complicated_name = ...
    # complicated_name has no taint outside nested functions, no issue
    _test_sink(complicated_name)

    # model capturing all variables as parameter sources in nested functions
    def model_all_captured_as_parameter_sources():
        _test_sink(complicated_name)

    model_all_captured_as_parameter_sources()
    _test_sink(complicated_name)  # no issue


def captured_variable_model_generation_source():
    complicated_name = ...

    # model simulates writing taint to nonlocal
    def model_all_captured_as_generation_sources():
        complicated_name
        _test_sink(complicated_name)  # no issue

    model_all_captured_as_generation_sources()
    _test_sink(complicated_name)


def captured_variable_model_both_generation_parameter_source():
    complicated_name = ...

    # model simulates writing taint to nonlocal and parameter sources
    def model_all_captured_as_generation_and_parameter_sources():
        _test_sink(complicated_name)

    model_all_captured_as_generation_and_parameter_sources()
    _test_sink(complicated_name)


def captured_variable_model_sink():
    complicated_name = _test_source()

    # model simulates sink on each captured variable
    def model_all_captured_as_sinks():
        complicated_name

    model_all_captured_as_sinks()

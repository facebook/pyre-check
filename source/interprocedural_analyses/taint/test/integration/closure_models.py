# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


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
    pass


def outer():
    pass


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


def test_tito_transform():
    source = _test_source()
    def return_model_taint_tito():
        use_source(source)

    value = reclassify(
        inner=return_model_taint_tito,
        feature="breadcrumb1",
    )
    value.reclassify(feature="breadcrumb2")


def captured_variable_models():
    complicated_name = ...
    # complicated_name has no taint outside nested functions, no issue
    _test_sink(complicated_name)

    def model_all_captured_as_sources():
        # TODO(T180817012): Support capturing all variables as sources in nested functions
        _test_sink(complicated_name)

    def model_all_captured_as_sink():
        # TODO(T180817051): Support making all writes to captured variables as sinks
        nonlocal complicated_name
        complicated_name = "tainted"

    model_all_captured_as_sink()
    # TODO(T180817051): Support making all writes to captured variables as sinks
    _test_sink(complicated_name)

    complicated_name = _test_source()
    def model_all_captured_as_tito():
        complicated_name

    _test_sink(model_all_captured_as_tito())

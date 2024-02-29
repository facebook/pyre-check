# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def inner_sink_wrapper():
    def inner_sink(x):
        pass
    # TODO(T179041949): Find flow through inner function model
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

    # TODO(T180807435): Model flow from source to return of `return_model_taint_tito`
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

    # TODO(T180807435): Model flow from source to return of `return_model_taint_tito`
    value = reclassify(
        inner=return_model_taint_tito,
        feature="breadcrumb1",
    )
    value.reclassify(feature="breadcrumb2")

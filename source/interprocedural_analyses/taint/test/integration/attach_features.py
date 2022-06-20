# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def source():
    return 0


def source_with_inferred():
    a = source()
    return a


def inferred_is_propagated():
    return source_with_inferred()


def inferred_sink(taint_left, taint_right, taint_without_feature, untainted):
    _test_sink(taint_left)
    _test_sink(taint_right)
    _test_sink(taint_without_feature)


def sink_is_propagated(argument):
    inferred_sink(argument, None, None, None)


def taint_in_taint_out(arg):
    return arg


def tito_and_sink(arg):
    _test_sink(arg)
    return arg


def tito_is_propagated(arg):
    return taint_in_taint_out(arg)


def attach_without_tito(arg):
    return 0


def no_tito(arg):
    return attach_without_tito(arg)


def modeled_sink_with_optionals(a: int = 0, b: int = 1) -> None:
    _test_sink(b)


class HasMethods:
    def method_with_optionals(self, a: int = 0, b: int = 1) -> None:
        _test_sink(b)


def attach_to_returned_sink():
    x = _test_source()
    return x


def attach_to_returned_source():
    return 0


def attach_to_returned_source_ports():
    return 0


def attach_to_returned_source_2():
    return 0

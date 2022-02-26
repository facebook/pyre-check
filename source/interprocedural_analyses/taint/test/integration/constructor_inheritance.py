# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Integration test illustrating flows through an inherited constructor
from builtins import _test_sink, _test_source


class MyBaseClass:
    def __init__(self, argument: str) -> None:
        _test_sink(argument)


class MyDerivedClass(MyBaseClass):
    variable = ""


def test() -> None:
    # This flow should be detected:
    derived = MyDerivedClass(_test_source())

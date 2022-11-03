# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from integration_test.taint import source, sink


class ParentWithConstructor:
    def __init__(self, arg):
        ...


class ChildWithParentConstructor(ParentWithConstructor):
    def __init__(self, arg):
        super(ChildWithParentConstructor, self).__init__(arg)


class ParentWithoutConstructor:
    ...


class ChildWithoutParentConstructor(ParentWithoutConstructor):
    def __init__(self, arg):
        super(ChildWithoutParentConstructor, self).__init__(arg)


def test1():
    tainted = source()
    child = ChildWithParentConstructor(tainted)
    # This IS detected
    sink(child.arg)


def test2():
    tainted = source()
    child = ChildWithoutParentConstructor(tainted)
    # This IS NOT detected
    sink(child.arg)

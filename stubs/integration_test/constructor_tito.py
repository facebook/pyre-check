# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from django.http import HttpRequest


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


def test1(request: HttpRequest):
    tainted = request.GET["test"]
    child = ChildWithParentConstructor(tainted)
    # This IS detected
    eval(child.arg)


def test2(request: HttpRequest):
    tainted = request.GET["test"]
    child = ChildWithoutParentConstructor(tainted)
    # This IS NOT detected
    eval(child.arg)

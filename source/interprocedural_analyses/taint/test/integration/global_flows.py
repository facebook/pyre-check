# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_source


def update_arg1(arg1, arg2):
    ...


def update_arg2(arg1, arg2):
    ...


x = 1


def update_x_at_arg1():
    update_arg1(x, _test_source())


def unaffected_x_at_arg1():
    update_arg1(x, "not a taint source")


def update_x_at_arg2():
    update_arg2(_test_source(), x)


def unaffected_x_at_arg2():
    update_arg2("not a taint source", x)


def indirectly_update_x_arg1(arg):
    update_arg1(x, arg)


def x_tainted_indirectly_arg1():
    indirectly_update_x_arg1(_test_source())


def x_not_tainted():
    indirectly_update_x_arg1(1)


def indirectly_update_x_arg2(arg):
    update_arg2(arg, x)


def x_tainted_indirectly_arg2():
    indirectly_update_x_arg2(_test_source())


class MyList:
    def append(self, item):
        ...


l: MyList = ...


def append_directly():
    l.append(_test_source())


def append_argument(arg):
    l.append(arg)


def append_indirectly():
    append_argument(_test_source())

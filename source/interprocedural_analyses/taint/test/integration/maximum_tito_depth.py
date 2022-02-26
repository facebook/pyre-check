# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from builtins import _test_sink, _test_source


def tito_zero(x):
    return x


def tito_one(x):
    return tito_zero(x)


def tito_two(x):
    return tito_one(x)


def tito_three(x):
    return tito_two(x)


def tito_max_consecutive(x):
    a = tito_zero(x)
    b = tito_two(a)
    c = tito_one(b)
    return c  # Expected depth: 1 + max(0, 2, 1) = 3


def tito_min_disjoint(x, y):
    if x:
        return tito_zero(x)
    else:
        return tito_one(x)
    # Expected depth: 1 + min(0, 1) = 1


def tito_min_disjoint_max_consecutive(x, y):
    if y:
        a = tito_one(x)
        b = tito_zero(a)
    else:
        a = tito_two(x)
        b = tito_zero(a)
    return b  # Expected depth 2


class C:
    def tito(self, parameter):
        ...


def tito_obscure(x):
    # Obscure calls are treated as tito depth 0.
    c = C()
    return c.tito(x)


def tito_four(x):
    # Ignored because too far.
    return tito_three(x)


def issue():
    x = _test_source()
    y = tito_three(x)
    _test_sink(y)


def non_issue():
    x = _test_source()
    y = tito_four(x)
    _test_sink(y)

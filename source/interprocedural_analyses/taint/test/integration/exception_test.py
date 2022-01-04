# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def test_parameter_flow(ex: Exception):
    return str(ex)


def test_constructed_exception():
    ex = Exception("message")
    return str(ex)


def test_caught_exception():
    try:
        return ""
    except Exception as ex:
        return str(ex)


def none_throws(x):
    if x is None:
        raise Exception("none")
    return x


def test_sink_in_finally(x):
    try:
        return none_throws(x)
    finally:
        _test_sink(x)


def test_before_try_to_finally():
    x = _test_source()
    try:
        return none_throws(x)
    finally:
        _test_sink(x)


def test_within_try_to_finally():
    x = None
    try:
        x = _test_source()
        return none_throws(x)
    finally:
        # TODO(T106611060): We do not find the issue here.
        _test_sink(x)


def test_except_to_finally():
    x = None
    try:
        return none_throws(x)
    except:
        x = _test_source()
    finally:
        _test_sink(x)


def test_return_finally():
    try:
        print("test")
    finally:
        return _test_source()


def test_return_twice_finally():
    try:
        return "hello"
    finally:
        return _test_source()


def test_return_overrides_finally():
    try:
        # TODO(T106611060): We should discard the source here,
        # since the return in `finally` overrides it.
        return _test_source()
    finally:
        return "hello"

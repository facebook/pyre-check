# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from contextlib import contextmanager


class SourceOnEnter:
    def __init__(self):
        self.value = ""

    def __enter__(self):
        self.value = _test_source()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        return


def test_source_on_enter():
    c = SourceOnEnter()
    with c:
        _test_sink(c.value)  # Issue here.


def test_source_on_enter_as():
    with SourceOnEnter() as c:
        _test_sink(c.value)  # Issue here.


def test_source_on_enter_clear():
    c = SourceOnEnter()
    with c:
        c.value = ""
    
    _test_sink(c.value)  # No issue.


class SourceOnExit:
    def __init__(self):
        self.value = ""

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.value = _test_source()


def test_source_on_exit():
    c = SourceOnExit()
    with c:
        _test_sink(c.value)  # No issue.

    _test_sink(c.value)  # TODO(T228882237): False negative


class SinkOnEnter:
    def __init__(self, value):
        self.value = value

    def __enter__(self):
        _test_sink(self.value)
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        return


def test_sink_on_enter(x):  # Inferred sink on x
    c = SinkOnEnter(x)
    with c:
        c.value = ""


@contextmanager
def source_on_enter_and_sink_on_exit(x):
    try:
        yield _test_source()
    finally:
        _test_sink(x)


def test_generator_source_on_enter():
    with source_on_enter_and_sink_on_exit("") as x:
        _test_sink(x)  # Issue here.


def test_generator_sink_on_exit():
    with source_on_enter_and_sink_on_exit(_test_source()): # Issue here.
        _test_sink("")

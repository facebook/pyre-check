# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from dataclasses import dataclass

from pysa import _test_sink, _test_source


def test_match_equality():
    x = _test_source()
    match x:
        case "test":
            _test_sink(x)  # Issue.
        case _:
            _test_sink(x)  # Issue.


def test_match_list_pattern():
    x = _test_source()
    match x:
        case [a]:
            _test_sink(a)  # Issue.
        case [a, b]:
            _test_sink(a)  # Issue.
            _test_sink(b)  # Issue.
        case [a, *l]:
            _test_sink(a)  # Issue.
            _test_sink(l)  # Issue.

    y = ["not_tainted", _test_source()]
    match y:
        case [a]:
            _test_sink(a)  # No issue.
        case [a, b]:
            _test_sink(a)  # No issue.
            _test_sink(b)  # Issue.
        case [a, *l]:
            _test_sink(a)  # No issue.
            _test_sink(l)  # Issue (false positive since [a, b] above matched).


def condition(x) -> bool: ...


def test_match_list_pattern_with_condition():
    x = _test_source()
    match x:
        case [a] if condition(a):
            _test_sink(a)  # Issue.


def test_match_mapping_pattern():
    x = _test_source()
    match x:
        case {"a": a}:
            _test_sink(a)  # Issue.
        case {"a": a, "b": b}:
            _test_sink(a)  # Issue.
            _test_sink(b)  # Issue.
        case {"a": a, **rest}:
            _test_sink(a)  # Issue.
            _test_sink(rest)  # Issue.

    d = {"b": _test_source()}
    match d:
        case {"a": a}:
            _test_sink(a)  # No issue.
        case {"a": a, "b": b}:
            _test_sink(a)  # No issue.
            _test_sink(b)  # Issue.
        case {"a": a, **rest}:
            _test_sink(a)  # No issue.
            # Issue (false positive, since pattern above always matches)
            _test_sink(rest)


@dataclass
class MyClass:
    a: str = ""
    b: str = ""
    c: str = ""


def test_match_class_pattern():
    x = _test_source()
    match x:
        case MyClass(a):
            _test_sink(a)  # Issue.
        case MyClass(b, a=""):
            # pyrefly: ignore[unbound-name]
            _test_sink(a)  # Issue.
            _test_sink(b)  # Issue.
        case MyClass(a, b, c):
            _test_sink(a)  # Issue.
            _test_sink(b)  # Issue.
            _test_sink(c)  # Issue.

    o = MyClass(a="", b=_test_source(), c="")
    match o:
        case MyClass(a, b, c):
            _test_sink(a)  # No issue (currently a false positive)
            _test_sink(b)  # Issue.
            _test_sink(c)  # No issue (currently a false positive).

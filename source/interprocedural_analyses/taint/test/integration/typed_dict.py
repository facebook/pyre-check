# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
from typing import TypedDict


class SimpleTypedDict(TypedDict):
    foo: int
    bar: str


def test_typed_dict_setitem():
    d: SimpleTypedDict = {"foo": 0, "bar": ""}
    d["bar"] = _test_source()
    _test_sink(d["bar"])  # This is an issue.
    _test_sink(d["foo"])  # This is NOT an issue.


def test_typed_dict_constructor():
    d = SimpleTypedDict(foo=0, bar=_test_source())
    _test_sink(d["bar"])  # This is an issue.
    _test_sink(d["foo"])  # This is NOT an issue.

    d = SimpleTypedDict(foo=0, bar={"a": _test_source()})
    _test_sink(d["bar"]["a"])  # This is an issue.
    _test_sink(d["bar"]["b"])  # This is NOT an issue.
    _test_sink(d["foo"]["a"])  # This is NOT an issue.
    _test_sink(d["foo"]["b"])  # This is NOT an issue.

    d = SimpleTypedDict({"foo": 0, "bar": _test_source()})
    _test_sink(d["bar"])  # This is an issue.
    _test_sink(d["foo"])  # This is an issue (false positive).

    d = SimpleTypedDict({_test_source(): 0})
    _test_sink(d.keys())  # This is an issue.
    _test_sink(d["foo"])  # This is NOT an issue.
    _test_sink(d["bar"])  # This is NOT an issue.

    d = SimpleTypedDict([("foo", 0), ("bar", _test_source())])
    _test_sink(d["bar"])  # This is an issue.
    _test_sink(d["foo"])  # This is an issue (false positive).


class SanitizedFieldTypedDict(TypedDict):
    sanitized: str
    safe: str


class NestedTypedDict(TypedDict):
    genuine: int
    nested: SanitizedFieldTypedDict


def test_sanitize_field():
    d: NestedTypedDict = _test_source()
    _test_sink(d["genuine"])

    d: NestedTypedDict = _test_source()
    # TODO(T81192268): this should not trigger an issue.
    _test_sink(d["nested"]["sanitized"])

    bar: NestedTypedDict = _test_source()
    _test_sink(bar["nested"]["safe"])

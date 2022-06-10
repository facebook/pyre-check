# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import collections
from builtins import _test_sink, _test_source
from typing import NamedTuple


class MyNamedTuple(NamedTuple):
    benign: int
    bad: str


def tainted_tuple() -> MyNamedTuple:
    return MyNamedTuple(bad=_test_source(), benign=1)


def issue_with_bad():
    a = tainted_tuple()
    _test_sink(a.bad)


def no_issue_with_benign():
    a = tainted_tuple()
    _test_sink(a.benign)


OldSchoolNamedTuple = collections.namedtuple("OldSchoolNamedTuple", "benign bad")


def tainted_old_tuple():
    return OldSchoolNamedTuple(bad=_test_source(), benign=1)


def issue_with_old_school_named_tuples():
    a = tainted_old_tuple()
    _test_sink(a.bad)


def no_issue_with_old_school_named_tuples():
    a = tainted_old_tuple()
    _test_sink(a.benign)


class InheritedNamedTuple(MyNamedTuple):
    pass


def inherited_tuple():
    return InheritedNamedTuple(bad=_test_source(), benign=1)


def issue_with_inherited_named_tuple():
    a = inherited_tuple()
    _test_sink(a.bad)


def no_issue_with_benign_in_inherited_named_tuple():
    a = inherited_tuple()
    _test_sink(a.benign)


def aliased_indicies_forward():
    a = tainted_tuple()
    _test_sink(a[0])  # No issue
    _test_sink(a[1])  # Issue here
    _test_sink(a[2])  # Invalid attribute access


def aliased_indicies_forward_unknown_attribute(i: int):
    a = tainted_tuple()
    return a[i]  # Unknown attribute access


def aliased_indicies_backward(a: MyNamedTuple):
    _test_sink(a.benign)
    _test_sink(a[1])
    _test_sink(a[2])


def aliased_indicies_backward_unknown_attribute(a: MyNamedTuple, i: int):
    _test_sink(a[i])


class NamedTupleWithTaintedAttribute(NamedTuple):
    benign: int
    bad: str


def issue_with_named_tuple_with_tainted_attribute():
    NamedTupleWithTaintedAttribute(bad=_test_source(), benign=1)

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import collections
from builtins import __test_sink, __test_source
from typing import NamedTuple


class MyNamedTuple(NamedTuple):
    benign: int
    bad: str


def tainted_tuple() -> MyNamedTuple:
    return MyNamedTuple(bad=__test_source(), benign=1)


def issue_with_bad():
    a = tainted_tuple()
    __test_sink(a.bad)


def no_issue_with_benign():
    a = tainted_tuple()
    __test_sink(a.benign)


OldSchoolNamedTuple = collections.namedtuple("OldSchoolNamedTuple", "benign bad")


def tainted_old_tuple():
    return OldSchoolNamedTuple(bad=__test_source(), benign=1)


def issue_with_old_school_named_tuples():
    a = tainted_old_tuple()
    __test_sink(a.bad)


def no_issue_with_old_school_named_tuples():
    a = tainted_old_tuple()
    __test_sink(a.benign)


class InheritedNamedTuple(MyNamedTuple):
    pass


def inherited_tuple():
    return InheritedNamedTuple(bad=__test_source(), benign=1)


def issue_with_inherited_named_tuple():
    a = inherited_tuple()
    __test_sink(a.bad)


def no_issue_with_benign_in_inherited_named_tuple():
    a = inherited_tuple()
    __test_sink(a.benign)

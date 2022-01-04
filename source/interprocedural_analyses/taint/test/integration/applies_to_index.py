# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink


def only_applies_to_first():
    return 1, 0


def only_applies_to_second():
    return 0, 1


def only_applies_to_nested():
    return ((0, 1), (0, 0))


def issue_only_with_first():
    issue, no_issue = only_applies_to_first()
    _test_sink(issue)
    _test_sink(no_issue)


def issue_only_with_second():
    no_issue, issue = only_applies_to_second()
    _test_sink(no_issue)
    _test_sink(issue)


def issue_only_with_nested_first():
    first, second = only_applies_to_nested()
    a, issue = first
    c, d = second
    _test_sink(issue)
    _test_sink(a)
    _test_sink(c)
    _test_sink(d)
    return only_applies_to_nested()


def only_applies_to_a_key():
    return {"a": 1}


def issue_only_with_a_key():
    d = only_applies_to_a_key()
    _test_sink(d["a"])
    _test_sink(d["b"])


def only_applies_to_a_member():
    ...


def issue_with_member():
    x = only_applies_to_a_member()
    _test_sink(x.a)
    _test_sink(x.b)

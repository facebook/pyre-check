# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from builtins import _test_source


def propagate_sink_format_string(a):
    f"<{a}>"


def inline_issue_format_string():
    a = _test_source()
    f"<{a}>"
    f"{a}"


def propagate_sink_dot_format(a):
    "<{}>".format(a)


def inline_issue_dot_format():
    a = _test_source()
    "<{}>".format(a)


def propagate_sink_percent_format(a):
    "<%s>" % (a,)


def inline_issue_percent_format():
    a = _test_source()
    "<%s>" % (a,)


def propagate_sink_rhs_add_literal(a):
    "https://" + a


def inline_issue_rhs_add_literal():
    a = _test_source()
    "https://" + a


def propagate_sink_lhs_add_literal(a):
    sql_start = "SELECT"
    columns = a + " FROM"


def inline_issue_lhs_add_literal():
    a = _test_source()
    sql_start = "SELECT"
    columns = a + " FROM"


def inline_issue_format_string_proper_tito():
    a, b, c = _test_source(), "", _test_source()
    f"<{a}{b}{c}>"

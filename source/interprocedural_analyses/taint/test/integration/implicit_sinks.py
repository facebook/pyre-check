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


https_start = "https://"


def propagate_sink_add_global(a):
    https_start + a


def propagate_sink_lhs_add_literal(a):
    columns = a + " FROM"


def inline_issue_lhs_add_literal():
    a = _test_source()
    columns = a + " FROM"


def inline_issue_format_string_proper_tito():
    a, b, c = _test_source(), "", _test_source()
    f"<{a}{b}{c}>"


def implicit_sink_before_source():
    # TODO(T138308554): False negative of implicit sink declaration before use
    a = "<{}>"
    a.format(_test_source())


def implicit_sink_before_parameter(y):
    # TODO(T138308554): False negative, should back propagate y leads to implicit sink
    a = "<{}>"
    a.format(y)


def format_wrapper(a, y):
    # TODO(T138308718): False negative, most general solution needing conditional sinks
    a.format(y)


def conditional_literal_sink():
    y = _test_source()
    a = "<{}>"
    format_wrapper(a, y)


def string_literal_arguments_sink(template: str):
    x = _test_source()
    if 1 == 1:
        template.format("https://1", x)
    elif 1 == 1:
        template % ("https://2", x)  # TODO(T146946806): Should see an issue
    else:
        x + "https://3"


def string_literal_arguments_issue():
    string_literal_arguments_sink(_test_source())  # Should see an issue

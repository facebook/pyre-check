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
    # TODO(T88183535): Should be propogated, but isn't
    "<{}>".format(a)


def inline_issue_dot_format():
    a = _test_source()
    # TODO(T88183535): Should be caught, but isn't
    "<{}>".format(a)


def propagate_sink_percent_format(a):
    # TODO(T88183535): Should be propogated, but isn't
    "<%s>" % (a,)


def inline_issue_percent_format():
    a = _test_source()
    # TODO(T88183535): Should be caught, but isn't
    "<%s>" % (a,)

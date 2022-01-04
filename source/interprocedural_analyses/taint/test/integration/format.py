# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def source_via_format():
    taint = _test_source()
    return f"{taint} is bad"


def source_in_format():
    return f"{_test_source()} is bad"


def tito_via_format(arg1, arg2, arg3):
    return f"{arg1} and {arg2} but not arg3"


def sink_via_format(arg):
    _test_sink(f"{arg}")


def sink_in_format(arg):
    f"{_test_sink(arg)} is bad"


def issue_in_format():
    taint = _test_source()
    return f"{_test_sink(taint)} is bad"

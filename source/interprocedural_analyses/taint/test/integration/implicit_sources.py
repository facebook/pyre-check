# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa


def foo():
    return "123.456.789.123"


def bar_format_strings():
    user_controlled = 1
    return f"{user_controlled}:123.456.789.123"


def bar_percent_format():
    user_controlled = 1
    return "%s:123.456.789.123" % (user_controlled,)


def bar_dot_format():
    user_controlled = 1
    return "{}:123.456.789.123".format(user_controlled)


def does_not_match():
    return "123.456"


def multiple_patterns():
    return "<123.456.789.123>"

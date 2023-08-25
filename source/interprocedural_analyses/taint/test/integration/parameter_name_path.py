# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source


def test_tito_regular_parameters(x, y, z):
    pass


def test_tito_args_kwargs(x, *args, **kwargs):
    pass


def test_tito_positional_only_parameter(__x, /):
    pass


def test_tito_keyword_only_parameter(x, *, y):
    pass


def test_tito_mix_positional_and_named_parameters(__x, /, y, *, z):
    pass

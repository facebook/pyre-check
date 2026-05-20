# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys

from pysa import _test_sink, _test_source

if sys.version_info.minor >= 10:
    pass
else:
    # Condition is always true, so this should not be reported.
    # Check that this is consistent between Pyre and Pyrefly.
    def version_specific_function(x):
        _test_sink(x)
        return _test_source()


def normal_function(x):
    _test_sink(x)
    return _test_source()

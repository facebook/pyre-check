# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
The language_server package contains the functionality powering the Pyre client's
IDE integrations. Handles connection logic and communicating LSP features.
"""

from . import connections, features  # noqa F401

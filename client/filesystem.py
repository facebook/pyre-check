# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module contains filesystem-related utility code including:
- path expansion tools
"""

from pathlib import Path


def expand_relative_path(root: str, path: str) -> str:
    expanded_path = Path(path).expanduser()
    if expanded_path.is_absolute():
        return str(expanded_path)
    else:
        return str(Path(root) / expanded_path)


def expand_global_root(path: str, global_root: str) -> str:
    if path.startswith("//"):
        return expand_relative_path(global_root, path[2:])
    return path

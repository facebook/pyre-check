# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module contains filesystem-related utility code including:
- validation of directory permissions and file existence
- path expansion tools
- file-lock handling code
"""

import logging
import os
from pathlib import Path


LOG: logging.Logger = logging.getLogger(__name__)


class EnvironmentException(Exception):
    pass


def readable_directory(directory: str) -> str:
    if not os.path.isdir(directory):
        raise EnvironmentException(f"`{directory}` is not a valid directory.")
    if not os.access(directory, os.R_OK):
        raise EnvironmentException(f"`{directory}` is not a readable directory.")
    return directory


def writable_directory(path: str) -> str:
    # Create the directory if it does not exist.
    try:
        os.makedirs(path)
    except FileExistsError:
        pass
    path = os.path.abspath(path)
    if not os.path.isdir(path):
        raise EnvironmentException(f"{path} is not a valid directory.")
    if not os.access(path, os.W_OK):
        raise EnvironmentException(f"{path} is not a writable directory.")
    return path


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


def file_or_directory_exists(path: str) -> str:
    if os.path.isdir(path) or os.path.isfile(path):
        return path
    raise ValueError(f"{path} is not a valid path")

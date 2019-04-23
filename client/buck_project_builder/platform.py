# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from typing import Tuple


def get_platform() -> str:
    # TODO(T38892701): Infer the platform more intelligently (platform007 is the
    # default, but it can be overridden depending on the project directory).
    return "platform007"


def get_python_version() -> Tuple[int, int]:
    # TODO(T38892701): Infer the Python version more intelligently.
    return (3, 0)


def parse_python_version(version_string: str) -> Tuple[int, int]:
    if "." in version_string:
        split = version_string.split(".")
        # Sometimes the version string contains a string prefix, e.g. "cinder.3.6".
        major, minor = split[-2:]
    else:
        major, minor = version_string, 0
    return (int(major), int(minor))

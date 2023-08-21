# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module contains specialized exceptions for when we encounter invalid
configurations.
"""


class InvalidConfiguration(Exception):
    def __init__(self, message: str) -> None:
        self.message = f"Invalid configuration: {message}"
        super().__init__(self.message)


class InvalidPythonVersion(InvalidConfiguration):
    def __init__(self, message: str) -> None:
        super().__init__(message)


class InvalidPackage(ValueError):
    def __init__(self, pkg_name: str) -> None:
        super().__init__(f"Invalid package: {pkg_name} does not exist.")

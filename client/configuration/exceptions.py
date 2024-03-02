# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

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

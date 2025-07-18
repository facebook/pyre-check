# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides some utility code for handling exit status and
uncaught exceptions in Pyre commands.
"""

import enum
import signal


class ExitCode(enum.IntEnum):
    SUCCESS = 0
    FOUND_ERRORS = 1
    FAILURE = 2
    BUCK_INTERNAL_ERROR = 3
    SERVER_NOT_FOUND = 4
    INCONSISTENT_SERVER = 5
    CONFIGURATION_ERROR = 6
    BUCK_USER_ERROR = 7
    WATCHMAN_ERROR = 8
    TAINT_CONFIGURATION_ERROR = 9
    MODEL_VERIFICATION_ERROR = 10
    UNSUPPORTED_PLATFORM = 11
    CLICK_EXCEPTION = 12
    # Exit should only happen after a shutdown message
    LANGUAGE_SERVER_EXIT = 13
    PYREFLY_FILE_FORMAT_ERROR = 14
    # If the process exited due to a signal, this will be the negative signal number.
    SIGSEGV = -signal.SIGSEGV


class ClientException(Exception):
    exit_code: ExitCode

    def __init__(self, message: str, exit_code: ExitCode = ExitCode.FAILURE) -> None:
        super().__init__(message)
        self.exit_code = exit_code

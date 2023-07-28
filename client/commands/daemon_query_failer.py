# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import re

from re import Pattern
from typing import Optional

from ..language_server import daemon_connection

from .daemon_query import DaemonQueryFailure


class AbstractDaemonQueryFailer(abc.ABC):
    @abc.abstractmethod
    def query_failure(
        self,
        path: str,
    ) -> Optional[DaemonQueryFailure]:
        """A result of None indicates that failure should not take place"""
        raise NotImplementedError()

    def query_connection_failure(
        self, path: str
    ) -> Optional[daemon_connection.DaemonConnectionFailure]:
        """A result of None indicates that failure should not take place"""
        raise NotImplementedError()


class DaemonQueryNoOpFailer(AbstractDaemonQueryFailer):
    def query_failure(self, path: str) -> Optional[DaemonQueryFailure]:
        return None

    def query_connection_failure(
        self, path: str
    ) -> Optional[daemon_connection.DaemonConnectionFailure]:
        return None


class RegexDaemonQueryFailer(AbstractDaemonQueryFailer):
    """Fails daemon queries matching a specified regex pattern"""

    def __init__(self, reject_regex: str) -> None:
        self.reject_regex = reject_regex
        self.compiled_reject_regex: Pattern[str] = re.compile(reject_regex)

    def _matches_regex(self, path: str) -> Optional[str]:
        if self.compiled_reject_regex.match(path):
            return f"Not querying daemon for path: {path} as matches regex: {self.reject_regex}"

    def query_failure(self, path: str) -> Optional[DaemonQueryFailure]:
        if (fail_message := self._matches_regex(path)) is not None:
            return DaemonQueryFailure(fail_message)

    def query_connection_failure(
        self, path: str
    ) -> Optional[daemon_connection.DaemonConnectionFailure]:
        if (fail_message := self._matches_regex(path)) is not None:
            return daemon_connection.DaemonConnectionFailure(fail_message)

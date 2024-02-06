# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import dataclasses

from typing import Optional

from ..language_server import daemon_connection


@dataclasses.dataclass
class DaemonFailerFailure:
    message: str


class AbstractDaemonQueryFailer(abc.ABC):
    @abc.abstractmethod
    def query_failure(
        self,
        path: str,
    ) -> Optional[DaemonFailerFailure]:
        """A result of None indicates that failure should not take place"""
        raise NotImplementedError()

    def query_connection_failure(
        self, path: str
    ) -> Optional[daemon_connection.DaemonConnectionFailure]:
        """A result of None indicates that failure should not take place"""
        raise NotImplementedError()


class DaemonQueryNoOpFailer(AbstractDaemonQueryFailer):
    def query_failure(self, path: str) -> Optional[DaemonFailerFailure]:
        return None

    def query_connection_failure(
        self, path: str
    ) -> Optional[daemon_connection.DaemonConnectionFailure]:
        return None

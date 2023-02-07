# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, Union


@dataclass
class StartedServerInfo:
    pass


@dataclass
class StartFailure:
    message: str


class DaemonLauncherConfiguration(abc.ABC):
    @abc.abstractmethod
    def get_global_root(self) -> Path:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_binary_location(self, download_if_needed: bool = False) -> Optional[Path]:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_remote_logger(self) -> Optional[str]:
        raise NotImplementedError()


class DaemonLauncher:
    """
    The DaemonLauncher is the API for starting up a pyre backend daemon. This API differs from the PyreDaemonLaunchAndSubscribeHandler since it is not a background task and will exit the subscription immediately once establishing a connection instead of sending LSP messages.
    It is also only for the code navigation server.
    """

    async def start_server(
        self,
        daemon_launcher_configuration: DaemonLauncherConfiguration,
    ) -> Union[StartedServerInfo, StartFailure]:
        return StartFailure("Unimplemented")

    async def stop_server(
        self,
    ) -> None:
        """Stops the server completely. If any other clients are relying on this server as well, it will kill their connection so use sparingly."""
        raise NotImplementedError()

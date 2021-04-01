# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import time
from logging import Logger
from typing import List, Optional

from .. import command_arguments, configuration_monitor, filesystem, watchman
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..project_files_monitor import ProjectFilesMonitor
from .command import ClientException, Command, State


LOG: Logger = logging.getLogger(__name__)


class Stop(Command):
    NAME = "stop"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        from_restart: bool = False,
    ) -> None:
        super(Stop, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._from_restart = from_restart

    def _flags(self) -> List[str]:
        flags = []
        flags.extend(["-log-directory", self._configuration.log_directory])
        return flags

    def _pid_file(self) -> Optional[int]:
        try:
            with open(
                os.path.join(self._configuration.log_directory, "server", "server.pid")
            ) as pid_file:
                return int(pid_file.read())
        except (OSError, ValueError):
            return None

    def _stop(self) -> None:
        if self._state() == State.DEAD:
            if self._from_restart:
                LOG.info("No server running.")
            else:
                LOG.warning("No server running.")
        else:
            pid_to_poll = self._pid_file()
            try:
                stopped = False
                # If this call fails, check() will throw a ClientException.
                self._call_client(command=self.NAME).check()
                # Poll for a second to ensure that the server has a chance to exit.
                if pid_to_poll is not None:
                    stop_time = time.time() + 1.0
                    LOG.info("Polling for server's process to stop...")
                    while time.time() < stop_time:
                        # send a null signal to validate the process's existence. If the
                        # process has terminated, a ProcessLookupError will be thrown.
                        os.kill(pid_to_poll, 0)
                        time.sleep(0.1)
                    stopped = True
            except ClientException:
                # An error was encountered when running `pyre stop`.
                stopped = False
            except ProcessLookupError:
                LOG.info("The server process has stopped.")
                stopped = True

            if not stopped:
                LOG.warning(
                    "Could not stop server. "
                    + "Run `pyre kill` if you want to force-kill all existing servers."
                )
            else:
                LOG.info("Stopped server at `%s`", self._analysis_directory.get_root())

        watchman.stop_subscriptions(
            ProjectFilesMonitor.base_path(self._configuration), ProjectFilesMonitor.NAME
        )
        watchman.stop_subscriptions(
            configuration_monitor.ConfigurationMonitor.base_path(self._configuration),
            configuration_monitor.ConfigurationMonitor.NAME,
        )

    def _run(self) -> None:
        LOG.info("Waiting for the client lock...")
        client_lock = os.path.join(self._configuration.log_directory, "client.lock")

        # Acquire the client lock to prevent a potential race with a background
        # `pyre start`.
        # Otherwise, `pyre stop` would stop some of the processes but not
        # others. For example, that may stop the configuration monitor but
        # leave the server alive, which would lead to errors saying "File
        # watching service is down".
        with filesystem.acquire_lock(client_lock, blocking=True):
            self._stop()

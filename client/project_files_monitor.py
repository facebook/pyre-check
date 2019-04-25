# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import functools
import logging
import os
import socket
import subprocess
import sys
from typing import Any, BinaryIO, Dict, Iterable, List, Optional, Set  # noqa

from . import language_server_protocol
from .configuration import Configuration
from .filesystem import AnalysisDirectory, find_root
from .watchman_subscriber import Subscription, WatchmanSubscriber


LOG = logging.getLogger(__name__)  # type: logging.Logger


class MonitorException(Exception):
    pass


class SocketConnection(object):
    def __init__(self, socket_path: str) -> None:
        self.socket = socket.socket(
            socket.AF_UNIX, socket.SOCK_STREAM
        )  # type: socket.socket
        self.socket.connect(socket_path)
        self.input = self.socket.makefile(mode="rb")  # type: BinaryIO
        self.output = self.socket.makefile(mode="wb")  # type: BinaryIO

    def close(self) -> None:
        try:
            self.socket.close()
        except OSError:
            pass

    def __del__(self) -> None:
        """
            Monitor is created and then runs in a forked process.
            As a result, in the original process, this object gets garbage
            collected without the socket being closed.
            For this reason, we explicitly close the socket on destruction.
        """
        self.close()


class Monitor(WatchmanSubscriber):
    NAME = "file_monitor"

    def __init__(
        self,
        arguments: argparse.Namespace,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        super(Monitor, self).__init__(analysis_directory)
        self._arguments = arguments
        self._configuration = configuration
        self._analysis_directory = analysis_directory

        self._extensions = set(
            ["py", "pyi"] + configuration.extensions
        )  # type: Set[str]

        self._watchman_path = self._find_watchman_path(
            arguments.current_directory
        )  # type: str

        socket_path = os.path.join(
            self._analysis_directory.get_root(), ".pyre", "server", "json_server.sock"
        )
        self._socket_connection = self._connect_to_socket(
            socket_path
        )  # type: SocketConnection

        try:
            language_server_protocol.perform_handshake(
                self._socket_connection.input,
                self._socket_connection.output,
                self._configuration.version_hash,
            )
        except (OSError, ValueError) as error:
            raise MonitorException(
                "Exception encountered during handshake: `{}`".format(error)
            )

    @property
    def _name(self) -> str:
        return self.NAME

    @property
    @functools.lru_cache(1)
    def _subscriptions(self) -> List[Subscription]:
        subscription = {
            "expression": [
                "allof",
                ["type", "f"],
                ["not", "empty"],
                ["anyof", *[["suffix", extension] for extension in self._extensions]],
            ],
            "fields": ["name"],
        }
        return [
            Subscription(
                self._watchman_path, "pyre_file_change_subscription", subscription
            )
        ]

    @staticmethod
    def pid_path(analysis_directory_root: str) -> str:
        return os.path.join(
            analysis_directory_root,
            ".pyre",
            Monitor.NAME,
            "{}.pid".format(Monitor.NAME),
        )

    @staticmethod
    def is_alive(analysis_directory_root: str) -> bool:
        pid_path = Monitor.pid_path(analysis_directory_root)
        try:
            with open(pid_path) as file:
                pid = int(file.read())
                os.kill(pid, 0)  # throws if process is not running
            return True
        except Exception:
            return False

    def _handle_response(self, response: Dict[str, Any]) -> None:
        try:
            absolute_paths = [
                os.path.join(response["root"], path) for path in response["files"]
            ]
            LOG.info("Received Watchman update for files %s.", absolute_paths)

            updated_paths = self._analysis_directory.process_updated_files(
                absolute_paths
            )

            if not updated_paths:
                LOG.info("Skipping update: Pyre doesn't track any of these files.")
                return

            LOG.info("Notifying server of update to files %s.", updated_paths)
            message = language_server_protocol.LanguageServerProtocolMessage(
                method="updateFiles", parameters={"files": updated_paths}
            )
            if not language_server_protocol.write_message(
                self._socket_connection.output, message
            ):
                LOG.info("Failed to communicate with server. Shutting down.")
                self._alive = False  # terminate daemon
                self._socket_connection.close()
        except KeyError:
            pass

    @staticmethod
    def _find_watchman_path(directory: str) -> str:
        watchman_path = find_root(directory, ".watchmanconfig")
        if not watchman_path:
            raise MonitorException(
                "Could not find a watchman directory from "
                "the current directory `{}`".format(directory)
            )
        return watchman_path

    @staticmethod
    def _connect_to_socket(socket_path: str) -> SocketConnection:
        try:
            return SocketConnection(os.path.realpath(socket_path))
        except (ConnectionRefusedError, FileNotFoundError, OSError) as error:
            raise MonitorException(
                "Failed to connect to server at `{}`. Reason: `{}`".format(
                    socket_path, error
                )
            )

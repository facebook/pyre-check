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
from .filesystem import AnalysisDirectory, find_paths_with_extensions, find_root
from .watchman_subscriber import Subscription, WatchmanSubscriber


LOG = logging.getLogger(__name__)  # type: logging.Logger


class ProjectFilesMonitorException(Exception):
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
            ProjectFilesMonitor is created and then runs in a forked process.
            As a result, in the original process, this object gets garbage
            collected without the socket being closed.
            For this reason, we explicitly close the socket on destruction.
        """
        self.close()


class ProjectFilesMonitor(WatchmanSubscriber):
    def __init__(
        self,
        arguments: argparse.Namespace,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        super(ProjectFilesMonitor, self).__init__(analysis_directory)
        self._arguments = arguments
        self._configuration = configuration
        self._analysis_directory = analysis_directory

        self._extensions = set(
            ["py", "pyi"] + configuration.extensions
        )  # type: Set[str]

        self._watchman_path = self._find_watchman_path(
            arguments.current_directory
        )  # type: str

        # Mapping from (actual) source files in the project root to symbolic
        # links in the analysis directory
        self._symbolic_links = self._calculate_symbolic_links(
            self._analysis_directory.get_root(), self._extensions
        )  # type: Dict[str, str]

        tracked_directories = [
            self._analysis_directory.get_root(),
            *[
                os.path.join(*path.split("$"))
                for path in self._configuration.search_path
            ],
        ]
        self._tracked_directories = [
            os.path.abspath(path) for path in tracked_directories
        ]  # type: List[str]

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
            raise ProjectFilesMonitorException(
                "Exception encountered during handshake: `{}`".format(error)
            )

    @property
    def _name(self) -> str:
        return "file_monitor"

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

    def _update_symbolic_links(self, paths: Iterable[str]) -> None:
        """
            For new/deleted files, attempt to keep the symbolic link mapping
            consistent.
        """
        # If any paths are within the directories Pyre understands (not a separate
        # buck directory), we can add them to the set of tracked files.
        for path in paths:
            if any(
                path.startswith(directory + os.sep)
                for directory in self._tracked_directories
            ):
                try:
                    real_path = os.path.realpath(path)
                    self._symbolic_links[real_path] = path
                except OSError:
                    pass

    def _handle_response(self, response: Dict[str, Any]) -> None:
        try:
            updated_paths = response["files"]
            root = response["root"]
            absolute_paths = [os.path.join(root, path) for path in updated_paths]
            self._update_symbolic_links(absolute_paths)
            tracked_paths = filter(None, map(self._symbolic_links.get, absolute_paths))
            LOG.info("Received Watchman update for files %s", absolute_paths)
            message = language_server_protocol.LanguageServerProtocolMessage(
                method="updateFiles", parameters={"files": list(tracked_paths)}
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
    def _calculate_symbolic_links(
        directory: str, extensions: Iterable[str]
    ) -> Dict[str, str]:
        """
            Given an analysis directory (usually containing symbolic links to actual
            files), produce a mapping from actual source files to files contained
            within this directory. Only includes files which have one of the provided
            extensions.

            Watchman watches actual source files, so when a change is detected to a
            file, this mapping can be used to identify what file changed from Pyre's
            perspective.
        """
        symbolic_links = {}
        try:
            for symbolic_link in find_paths_with_extensions(directory, extensions):
                symbolic_links[os.path.realpath(symbolic_link)] = symbolic_link
        except subprocess.CalledProcessError as error:
            LOG.warning(
                "Exception encountered trying to find source files "
                "in the analysis directory: `%s`",
                error,
            )
            LOG.warning("Starting with an empty symlink map.")
        return symbolic_links

    @staticmethod
    def _find_watchman_path(directory: str) -> str:
        watchman_path = find_root(directory, ".watchmanconfig")
        if not watchman_path:
            raise ProjectFilesMonitorException(
                "Could not find a watchman directory from "
                "the current directory `{}`".format(directory)
            )
        return watchman_path

    @staticmethod
    def _connect_to_socket(socket_path: str) -> SocketConnection:
        try:
            return SocketConnection(socket_path)
        except (ConnectionRefusedError, FileNotFoundError, OSError) as error:
            raise ProjectFilesMonitorException(
                "Failed to connect to server at `{}`. Reason: `{}`".format(
                    socket_path, error
                )
            )

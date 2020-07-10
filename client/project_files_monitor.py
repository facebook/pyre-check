# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import functools
import os
from pathlib import Path
from typing import Any, Dict, List, Sequence, Set

from . import json_rpc, watchman
from .analysis_directory import AnalysisDirectory
from .buck import BuckException
from .configuration import Configuration
from .filesystem import find_root
from .process import Process
from .socket_connection import SocketConnection

# We use the `LOG` from watchman due to its better formatting in log files
from .watchman import LOG, Subscriber, Subscription


class MonitorException(Exception):
    pass


def _log_paths(message: str, paths: Sequence[str]) -> None:
    path_count = len(paths)
    log_threshold = 30
    if path_count <= log_threshold:
        LOG.info(f"{message} {paths}")
    else:
        additional_count = path_count - log_threshold
        LOG.info(f"{message} {paths[:log_threshold]} (and {additional_count} more)")


class ProjectFilesMonitor(Subscriber):
    """
        Logs from this monitor are found in
        .pyre/<local root>/file_monitor/file_monitor.log
        One file monitor is spawned per pyre server. When a server is stopped,
        the process of pyre file monitor associated to it is killed.
    """

    NAME = "file_monitor"

    def __init__(
        self,
        configuration: Configuration,
        project_root: str,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        super(ProjectFilesMonitor, self).__init__(self.base_path(configuration))
        self._configuration = configuration
        self._analysis_directory = analysis_directory

        self._extensions: Set[str] = set(
            ["py", "pyi", "thrift"] + configuration.extensions
        )

        self._watchman_path: str = self._find_watchman_path(project_root)

        self.socket_connection = SocketConnection(self._configuration.log_directory)
        self.socket_connection.connect()
        self.socket_connection.perform_handshake(self._configuration.version_hash)

    @property
    def _name(self) -> str:
        return self.NAME

    @property
    @functools.lru_cache(1)
    def _subscriptions(self) -> List[Subscription]:
        subscription = {
            "empty_on_fresh_instance": True,
            "expression": [
                "allof",
                ["type", "f"],
                [
                    "anyof",
                    *[["suffix", extension] for extension in self._extensions],
                    ["match", "TARGETS"],
                ],
            ],
            "fields": ["name"],
        }
        return [
            Subscription(
                self._watchman_path, "pyre_file_change_subscription", subscription
            )
        ]

    @staticmethod
    def base_path(configuration: Configuration) -> str:
        return os.path.join(configuration.log_directory, ProjectFilesMonitor.NAME)

    def _handle_response(self, response: Dict[str, Any]) -> None:
        try:
            absolute_paths = [
                os.path.join(response["root"], path) for path in response["files"]
            ]
            _log_paths("Received Watchman update for files", absolute_paths)

            updated_paths = self._analysis_directory.process_updated_files(
                absolute_paths
            )

            if updated_paths.is_empty():
                LOG.info("Skipping update: Pyre doesn't track any of these files.")
                return

            _log_paths(
                "Notifying server of update to files", updated_paths.updated_paths
            )
            _log_paths("  and invalidation of files", updated_paths.deleted_paths)

            message = json_rpc.Request(
                method="updateFiles",
                parameters={
                    "files": updated_paths.updated_paths,
                    "invalidated": updated_paths.deleted_paths,
                },
            )
            if not message.write(self.socket_connection.output):
                LOG.info("Failed to communicate with server. Shutting down.")
                self._alive = False  # terminate daemon
                self.socket_connection.close()

        except KeyError:
            pass

        except BuckException:
            LOG.info("Unable to build project.")
            pass

        except Exception as exception:
            LOG.info(f"Exception during handling of file update: {exception}")
            raise exception

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
    def restart_if_dead(
        configuration: Configuration,
        project_root: str,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        pid_path = watchman.compute_pid_path(
            ProjectFilesMonitor.base_path(configuration), ProjectFilesMonitor.NAME
        )
        if Process.is_alive(Path(pid_path)):
            return
        LOG.debug("File monitor is not running.")
        try:
            ProjectFilesMonitor(
                configuration, project_root, analysis_directory
            ).daemonize()
            LOG.debug("Restarted file monitor.")
        except MonitorException as exception:
            LOG.warning(f"Failed to restart file monitor: {exception}")

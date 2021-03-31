# Copyright (c) Facebook, Inc. and its affiliates.
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
from .find_directories import find_parent_directory_containing_file
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
        super(ProjectFilesMonitor, self).__init__(
            self.base_path(configuration), configuration
        )
        self._analysis_directory = analysis_directory

        self._extensions: Set[str] = set(
            ["py", "pyi", "thrift"]
            + [
                extension[1:]
                for extension in configuration.get_valid_extension_suffixes()
            ]
        )

        self._watchman_path: str = self._find_watchman_path(project_root)

        self.socket_connection = SocketConnection(self._configuration.log_directory)
        self.socket_connection.connect()
        self.socket_connection.perform_handshake(
            self._configuration.get_version_hash_respecting_override() or "unversioned"
        )

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
                parameters=json_rpc.ByNameParameters(
                    {
                        "files": updated_paths.updated_paths,
                        "invalidated": updated_paths.deleted_paths,
                    }
                ),
            )
            if not json_rpc.write_lsp_request(self.socket_connection.output, message):
                LOG.info("Failed to communicate with server. Shutting down.")
                self._alive = False  # terminate daemon
                self.socket_connection.close()

        except KeyError:
            pass

        except BuckException as exception:
            LOG.info(f"Unable to build project because of exception: `{exception}`.")

        except Exception as exception:
            LOG.info(f"Exception during handling of file update: {exception}")
            raise exception

    @staticmethod
    def _find_watchman_path(directory: str) -> str:
        watchman_path = find_parent_directory_containing_file(
            Path(directory), ".watchmanconfig"
        )
        if watchman_path is None:
            raise MonitorException(
                "Could not find a watchman directory from "
                + f"the current directory `{directory}`"
            )
        return str(watchman_path)

    @staticmethod
    def is_alive(configuration: Configuration) -> bool:
        pid_path = watchman.compute_pid_path(
            ProjectFilesMonitor.base_path(configuration), ProjectFilesMonitor.NAME
        )
        is_alive = Process.is_alive(Path(pid_path))
        if not is_alive:
            LOG.debug("The file monitor is down.")
        return is_alive

    def cleanup(self) -> None:
        LOG.info("Cleaning up the analysis directory.")
        self._analysis_directory.cleanup(delete_long_lasting_files=True)

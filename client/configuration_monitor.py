# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
from pathlib import Path
from typing import Any, Dict, List, Optional

from . import command_arguments, watchman
from .analysis_directory import AnalysisDirectory
from .commands import stop
from .configuration import Configuration
from .find_directories import CONFIGURATION_FILE, LOCAL_CONFIGURATION_FILE
from .process import Process

# We use the `LOG` from watchman due to its better formatting in log files
from .watchman import LOG, Subscriber, Subscription


class ConfigurationMonitor(Subscriber):
    """
    The ConfigurationMonitor watches only for .pyre_configuration(.local)
    files, and will kill the corresponding server when a configuration changes.
    Logs are found in
    .pyre/<local-root>/configuration_monitor/configuration_monitor.log
    To kill a monitor, get pid from
    .pyre/<local-root>/configuration_monitor/configuration_monitor.pid
    and kill <pid>.
    """

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
        project_root: str,
        original_directory: str,
        local_configuration_root: Optional[str],
        other_critical_files: List[str],
    ) -> None:
        super(ConfigurationMonitor, self).__init__(
            self.base_path(configuration), configuration
        )
        self._command_arguments = command_arguments
        self._analysis_directory = analysis_directory
        self._project_root_path: Path = Path(project_root).resolve()
        self._original_directory = original_directory
        self._local_configuration_root = local_configuration_root
        self._other_critical_files = other_critical_files

    NAME = "configuration_monitor"

    @property
    def _name(self) -> str:
        return ConfigurationMonitor.NAME

    @staticmethod
    def base_path(configuration: Configuration) -> str:
        return os.path.join(configuration.log_directory, ConfigurationMonitor.NAME)

    @property
    def _subscriptions(self) -> List[Subscription]:
        roots = self._watchman_client.query("watch-list")["roots"]
        LOG.debug(f"Watchman roots: {roots}")
        for root in roots:
            root_path = Path(root).resolve()
            if root_path in (self._project_root_path, *self._project_root_path.parents):
                name = f"pyre_monitor_{os.path.basename(root)}"
                subscription = {
                    "empty_on_fresh_instance": True,
                    "expression": [
                        "allof",
                        ["type", "f"],
                        [
                            "anyof",
                            ["suffix", "pyre_configuration.local"],
                            ["suffix", "pyre_configuration"],
                            *[
                                ["match", critical]
                                for critical in self._other_critical_files
                            ],
                        ],
                    ],
                    "fields": ["name"],
                }
                subscription = Subscription(root, name, subscription)
                LOG.debug(f"Configuration monitor subscription: {subscription}")
                return [subscription]
        LOG.debug("Configuration monitor is not subscribed to any paths.")
        return []

    def _stop(self) -> None:
        stop.Stop(
            self._command_arguments,
            self._original_directory,
            configuration=self._configuration,
            analysis_directory=self._analysis_directory,
        ).run()

    def _handle_response(self, response: Dict[str, Any]) -> None:
        watchman_root = response.get("root", None)
        paths = response.get("files", None)
        if watchman_root is None or paths is None:
            # Skip the Watchman response because it does not have both `root`
            # and `files`.
            return

        absolute_paths = [Path(watchman_root, path).resolve() for path in paths]
        LOG.info(f"Update to configuration or other critical files at {absolute_paths}")

        root_configuration_path = self._project_root_path / CONFIGURATION_FILE
        local_configuration_root = self._local_configuration_root
        if root_configuration_path in absolute_paths:
            LOG.info("Pyre configuration changed. Stopping pyre server.")
            # TODO(T54088045): Find all local pyre servers running underneath
            # and stop them.
            self._stop()
        elif (
            local_configuration_root is not None
            and Path(local_configuration_root, LOCAL_CONFIGURATION_FILE).resolve()
            in absolute_paths
        ):
            LOG.info("Local configuration changed. Stopping pyre server.")
            self._stop()
        elif any(
            self._project_root_path / critical in absolute_paths
            for critical in self._other_critical_files
        ):
            LOG.info("Critical file changed. Stopping pyre server.")
            self._stop()
        else:
            LOG.info(
                "None of the changed paths correspond to the root configuration, "
                + "a local pyre configuration, or any other critical file."
            )

    @staticmethod
    def is_alive(configuration: Configuration) -> bool:
        pid_path = watchman.compute_pid_path(
            ConfigurationMonitor.base_path(configuration), ConfigurationMonitor.NAME
        )
        is_alive = Process.is_alive(Path(pid_path))
        if not is_alive:
            LOG.debug("The configuration monitor is down.")
        return is_alive

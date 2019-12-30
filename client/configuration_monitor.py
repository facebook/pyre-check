# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import os
from logging import Logger
from pathlib import Path
from typing import Any, Dict, List, Optional

from .analysis_directory import AnalysisDirectory
from .commands import stop
from .configuration import CONFIGURATION_FILE, LOCAL_CONFIGURATION_FILE, Configuration
from .watchman_subscriber import Subscription, WatchmanSubscriber


LOG: Logger = logging.getLogger(__name__)


class ConfigurationMonitor(WatchmanSubscriber):
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
        arguments: argparse.Namespace,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
        project_root: str,
        original_directory: str,
        local_configuration_root: Optional[str],
    ) -> None:
        super(ConfigurationMonitor, self).__init__(self.base_path(configuration))
        self._arguments = arguments
        self._configuration = configuration
        self._analysis_directory = analysis_directory
        self._project_root_path: Path = Path(project_root).resolve()
        self._original_directory = original_directory
        self._local_configuration_root = local_configuration_root

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
                name = "pyre_monitor_{}".format(os.path.basename(root))
                subscription = {
                    "empty_on_fresh_instance": True,
                    "expression": [
                        "allof",
                        ["type", "f"],
                        ["not", "empty"],
                        [
                            "anyof",
                            ["suffix", "pyre_configuration.local"],
                            ["suffix", "pyre_configuration"],
                        ],
                    ],
                    "fields": ["name"],
                }
                subscription = Subscription(root, name, subscription)
                LOG.debug("Configuration monitor subscription: %s", subscription)
                return [subscription]
        LOG.debug("Configuration monitor is not subscribed to any paths.")
        return []

    def _handle_response(self, response: Dict[str, Any]) -> None:
        watchman_root = response.get("root", None)
        paths = response.get("files", None)
        if watchman_root is None or paths is None:
            # Skip the Watchman response because it does not have both `root`
            # and `files`.
            return

        absolute_paths = [Path(watchman_root, path).resolve() for path in paths]
        LOG.info("Update to configuration at %s", absolute_paths)

        root_configuration_path = self._project_root_path / CONFIGURATION_FILE
        local_configuration_root = self._local_configuration_root
        if root_configuration_path in absolute_paths:
            LOG.info("Pyre configuration changed. Stopping pyre server.")
            stop.Stop(
                self._arguments,
                self._original_directory,
                self._configuration,
                self._analysis_directory,
            ).run()
            # TODO(T54088045): Find all local pyre servers running underneath
            # and stop them.
        elif (
            local_configuration_root is not None
            and Path(local_configuration_root, LOCAL_CONFIGURATION_FILE).resolve()
            in absolute_paths
        ):
            LOG.info("Local configuration changed. Stopping pyre server.")
            stop.Stop(
                self._arguments,
                self._original_directory,
                self._configuration,
                self._analysis_directory,
            ).run()
        else:
            LOG.info(
                "None of the changed paths correspond to the root "
                "configuration or a local pyre configuration."
            )

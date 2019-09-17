# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import logging
import os
from typing import Any, Dict, List

from .commands import stop
from .configuration import CONFIGURATION_FILE
from .filesystem import AnalysisDirectory, is_parent
from .watchman_subscriber import Subscription, WatchmanSubscriber


LOG = logging.getLogger(__name__)


class ConfigurationMonitor(WatchmanSubscriber):
    """
        The ConfigurationMonitor watches only for .pyre_configuration(.local)
        files, and will kill the corresponding server and ProjectFileMonitor
        when a configuration changes.
        Logs are found in .pyre/configuration_monitor/configuration_monitor.log
        To kill a  monitor, get pid from
        .pyre/configuration_monitor/configuration_monitor.pid ; kill <pid>.
    """

    def __init__(
        self, arguments, configuration, analysis_directory: AnalysisDirectory
    ) -> None:
        super(ConfigurationMonitor, self).__init__(analysis_directory)
        self.arguments = arguments
        self.configuration = configuration
        self.analysis_directory = analysis_directory

    @property
    def _name(self) -> str:
        return "configuration_monitor"

    @property
    def _subscriptions(self) -> List[Subscription]:
        roots = self._watchman_client.query("watch-list")["roots"]
        names = ["pyre_monitor_{}".format(os.path.basename(root)) for root in roots]
        subscription = {
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
        return [
            Subscription(root, name, subscription) for (root, name) in zip(roots, names)
        ]

    def _handle_response(self, response: Dict[str, Any]) -> None:
        LOG.info(
            "Update to configuration at %s",
            os.path.join(response["root"], ",".join(response["files"])),
        )
        absolute_path = [
            os.path.join(response["root"], file) for file in response["files"]
        ]

        # Find the path to the project configuration file and compare it with the
        # list of changed configuration files.
        project_configuration = os.path.join(response["root"], CONFIGURATION_FILE)
        if any((project_configuration == file) for file in absolute_path):
            LOG.info("Pyre configuration changed. Stopping pyre server.")
            stop.Stop(self.arguments, self.configuration, self.analysis_directory).run()
            # TODO(T54088045): Find all local pyre servers running underneath
            # and stop them.
        else:
            LOG.info("None of the changed paths correspond to pyre configuration.")

        if self.arguments.local_configuration:
            if any(
                is_parent(self.arguments.local_configuration, file)
                for file in absolute_path
            ):
                LOG.info("Local configuration changed. Stopping pyre server.")
                stop.Stop(
                    self.arguments, self.configuration, self.analysis_directory
                ).run()
            else:
                LOG.info(
                    "None of the changed paths correspond to the current local"
                    "configuration."
                )

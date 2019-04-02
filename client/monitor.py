# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
from typing import Any, Dict, List

from .commands import stop
from .filesystem import AnalysisDirectory, is_parent
from .watchman_subscriber import Subscription, WatchmanSubscriber


LOG = logging.getLogger(__name__)


class Monitor(WatchmanSubscriber):
    def __init__(
        self, arguments, configuration, analysis_directory: AnalysisDirectory
    ) -> None:
        super(Monitor, self).__init__(analysis_directory)
        self.arguments = arguments
        self.configuration = configuration
        self.analysis_directory = analysis_directory

    @property
    def _name(self) -> str:
        return "monitor"

    @property
    def _subscriptions(self) -> List[Subscription]:
        roots = self._watchman_client.query("watch-list")["roots"]
        names = ["pyre_monitor_{}".format(os.path.basename(root)) for root in roots]
        subscription = {
            "expression": [
                "allof",
                ["type", "f"],
                ["not", "empty"],
                ["suffix", "pyre_configuration.local"],
            ],
            "fields": ["name"],
        }
        return [
            Subscription(root, name, subscription) for (root, name) in zip(roots, names)
        ]

    def _handle_response(self, response: Dict[str, Any]) -> None:
        LOG.info(
            "Update to local configuration at %s",
            os.path.join(response["root"], ",".join(response["files"])),
        )
        absolute_files = [
            os.path.join(response["root"], file) for file in response["files"]
        ]
        if any(
            is_parent(self.arguments.local_configuration, file)
            for file in absolute_files
        ):
            LOG.info("Stopping running pyre server.")
            stop.Stop(self.arguments, self.configuration, self.analysis_directory).run()
        else:
            LOG.info(
                "None of these paths correspond to the current local configuration."
            )

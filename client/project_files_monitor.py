# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import functools
import logging
import os
import subprocess
import sys
from typing import Any, Dict, Iterable, List, Optional, Set  # noqa

from .configuration import Configuration
from .filesystem import AnalysisDirectory, find_paths_with_extensions, find_root
from .watchman_subscriber import Subscription, WatchmanSubscriber


LOG = logging.getLogger(__name__)  # type: logging.Logger


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
        self._watchman_path = find_root(
            arguments.current_directory, ".watchmanconfig"
        )  # type: Optional[str]

        # Mapping from (actual) source files in the project root to symbolic
        # links in the analysis directory
        self._symbolic_links = self._calculate_symbolic_links(
            self._analysis_directory.get_root(), self._extensions
        )  # type: Dict[str, str]

    @property
    def _name(self) -> str:
        return "pyre_file_change_subscription"

    @property
    @functools.lru_cache(1)
    def _subscriptions(self) -> List[Subscription]:
        if not self._watchman_path:
            LOG.error(
                "Could not find a watchman directory from the current directory (%s)",
                self._arguments.current_directory,
            )
            # exit here after daemonized, so we do not terminate the main process
            sys.exit(0)

        subscription = {
            "expression": [
                "allof",
                ["type", "f"],
                ["not", "empty"],
                ["anyof", *[["suffix", extension] for extension in self._extensions]],
            ],
            "fields": ["name"],
        }
        return [Subscription(self._watchman_path, self._name, subscription)]

    # pyre-ignore: Dict[str, Any] allowed in strict on latest version
    def _handle_response(self, response: Dict[str, Any]) -> None:
        LOG.error("Received response from watchman: %s", response)

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
                "in the analysis directory: %s",
                error,
            )
            LOG.warning("Starting with an empty symlink map.")
        return symbolic_links

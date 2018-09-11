# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
from typing import List

from .. import filesystem
from .command import ExitCode
from .monitor import Monitor
from .reporting import Reporting


LOG = logging.getLogger(__name__)


class Start(Reporting):
    NAME = "start"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Start, self).__init__(arguments, configuration, analysis_directory)
        self._terminal = arguments.terminal
        self._no_watchman = arguments.no_watchman
        self._number_of_workers = configuration.number_of_workers

    def _run(self) -> None:
        while True:
            # Be optimistic in grabbing the lock in order to provide users with
            # a message when the lock is being waited on.
            blocking = False
            try:
                with filesystem.acquire_lock(".pyre/client.lock", blocking):
                    Monitor(
                        self._arguments, self._configuration, self._analysis_directory
                    ).daemonize()
                    # This unsafe call is OK due to the client lock always
                    # being acquired before starting a server - no server can
                    # spawn in the interim which would cause a race.
                    try:
                        with filesystem.acquire_lock(
                            os.path.join(
                                self._analysis_directory.get_root(),
                                ".pyre",
                                "server",
                                "server.lock",
                            ),
                            blocking=False,
                        ):
                            pass
                    except OSError:
                        LOG.warning(
                            "Server at `%s` exists, skipping.",
                            self._analysis_directory.get_root(),
                        )
                        self._exit_code = ExitCode.FAILURE
                        return

                    self._analysis_directory.prepare()
                    self._call_client(command=self.NAME).check()
                    return
            except OSError:
                blocking = True
                LOG.info("Waiting on the pyre client lock, pid %d.", os.getpid())

    def _flags(self) -> List[str]:
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        if len(filter_directories):
            flags.extend(
                ["-filter-directories-semicolon", ";".join(list(filter_directories))]
            )
        if not self._no_watchman:
            flags.append("-use-watchman")
        if self._terminal:
            flags.append("-terminal")
        flags.extend(
            [
                "-workers",
                str(self._number_of_workers),
                "-typeshed",
                self._configuration.typeshed,
                "-expected-binary-version",
                self._configuration.version_hash,
            ]
        )
        search_path = self._configuration.search_path
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])

        return flags

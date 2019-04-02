# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import logging
import os
from typing import List, Optional

from .. import filesystem, monitor, project_files_monitor
from .command import ExitCode
from .reporting import Reporting


LOG = logging.getLogger(__name__)


class Start(Reporting):
    NAME = "start"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Start, self).__init__(arguments, configuration, analysis_directory)
        self._terminal = arguments.terminal  # type: bool
        # TODO(T41488848) respect no_watchman flag after OCaml daemon phased out
        self._no_watchman = True  # type: bool
        self._number_of_workers = configuration.number_of_workers  # type: int
        self._configuration_file_hash = configuration.file_hash  # type: Optional[str]
        self._file_monitor = (
            None
        )  # type: Optional[project_files_monitor.ProjectFilesMonitor]
        if not arguments.no_saved_state:
            # Saved state.
            self._save_initial_state_to = (
                arguments.save_initial_state_to
            )  # type: Optional[str]
            self._changed_files_path = (
                arguments.changed_files_path
            )  # type: Optional[str]
            self._load_initial_state_from = (
                arguments.load_initial_state_from
            )  # type: Optional[str]
            self._saved_state_project = (
                arguments.saved_state_project
            )  # type: Optional[str]
        else:
            self._save_initial_state_to = None
            self._changed_files_path = None
            self._load_initial_state_from = None
            self._saved_state_project = None

    def _run(self) -> None:
        blocking = False
        while True:
            # Be optimistic in grabbing the lock in order to provide users with
            # a message when the lock is being waited on.
            try:
                with filesystem.acquire_lock(".pyre/client.lock", blocking):
                    if self._arguments.local_configuration:
                        monitor.Monitor(
                            self._arguments,
                            self._configuration,
                            self._analysis_directory,
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

                    try:  # TODO(T41488848) guard this block with a no_watchman check
                        self._file_monitor = project_files_monitor.ProjectFilesMonitor(
                            self._arguments,
                            self._configuration,
                            self._analysis_directory,
                        )
                        self._file_monitor.daemonize()
                        LOG.info("Initialized file monitor.")
                    except project_files_monitor.ProjectFilesMonitorException as error:
                        LOG.warning("Failed to initialize file monitor: %s", error)

                    return
            except OSError as exception:
                if exception.errno == errno.EAGAIN:
                    blocking = True
                    LOG.info("Waiting on the pyre client lock, pid %d.", os.getpid())
                else:
                    raise exception

    def _flags(self) -> List[str]:
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])
        if not self._no_watchman:
            flags.append("-use-watchman")
        if self._terminal:
            flags.append("-terminal")
        if self._save_initial_state_to and os.path.isdir(
            os.path.dirname(self._save_initial_state_to)
        ):
            flags.extend(["-save-initial-state-to", self._save_initial_state_to])
        if self._saved_state_project:
            flags.extend(["-saved-state-project", self._saved_state_project])
        if self._configuration_file_hash:
            flags.extend(["-configuration-file-hash", self._configuration_file_hash])
        if (
            self._load_initial_state_from is not None
            and self._changed_files_path is not None
        ):
            flags.extend(
                [
                    "-load-state-from",
                    self._load_initial_state_from,
                    "-changed-files-path",
                    self._changed_files_path,
                ]
            )
        elif (
            self._load_initial_state_from is None
            and self._changed_files_path is not None
        ):
            LOG.error(
                "--load-initial-state-from must be set if --changed-files-path is set."
            )
        elif (
            self._load_initial_state_from is not None
            and self._changed_files_path is None
        ):
            LOG.error(
                "--changed-files-path must be set if --load-initial-state-from is set."
            )
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

        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])

        extensions = self._configuration.extensions
        for extension in extensions:
            flags.extend(["-extension", extension])

        return flags

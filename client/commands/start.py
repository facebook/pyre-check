# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import logging
import os
from typing import List, Optional

from .. import filesystem, monitor, project_files_monitor
from .command import ExitCode, typeshed_search_path
from .reporting import Reporting


LOG = logging.getLogger(__name__)


class Start(Reporting):
    NAME = "start"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Start, self).__init__(arguments, configuration, analysis_directory)
        self._taint_models_path = configuration.taint_models_path  # type: List[str]
        self._terminal = arguments.terminal  # type: bool
        self._store_type_check_resolution = arguments.store_type_check_resolution
        self._use_watchman = not arguments.no_watchman  # type: bool
        self._transitive = arguments.transitive  # type: bool
        self._number_of_workers = configuration.number_of_workers  # type: int
        self._configuration_file_hash = configuration.file_hash  # type: Optional[str]
        self._file_monitor = None  # type: Optional[project_files_monitor.Monitor]
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

                    if self._use_watchman:
                        try:
                            self._file_monitor = project_files_monitor.Monitor(
                                self._arguments,
                                self._configuration,
                                self._analysis_directory,
                            )
                            self._file_monitor.daemonize()
                            LOG.info("Initialized file monitor.")
                        except project_files_monitor.MonitorException as error:
                            LOG.warning("Failed to initialize file monitor: %s", error)

                    return
            except OSError as exception:
                if exception.errno == errno.EAGAIN:
                    blocking = True
                    LOG.info("Waiting on the pyre client lock.")
                else:
                    raise exception

    def _flags(self) -> List[str]:
        flags = super()._flags()
        if self._taint_models_path:
            for path in self._taint_models_path:
                flags.extend(["-taint-models", path])
        filter_directories = self._get_directories_to_analyze()
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])
        if len(self._configuration.ignore_all_errors):
            flags.extend(
                [
                    "-ignore-all-errors",
                    ";".join(sorted(self._configuration.ignore_all_errors)),
                ]
            )
        if self._terminal:
            flags.append("-terminal")
        if self._store_type_check_resolution:
            flags.append("-store-type-check-resolution")
        if self._save_initial_state_to and os.path.isdir(
            os.path.dirname(self._save_initial_state_to)
        ):
            flags.extend(["-save-initial-state-to", self._save_initial_state_to])
        if self._saved_state_project:
            flags.extend(["-saved-state-project", self._saved_state_project])
            local_configuration_root = self._configuration.local_configuration_root
            if local_configuration_root is not None:
                relative = os.path.relpath(local_configuration_root)
                flags.extend(["-saved-state-metadata", relative.replace("/", "$")])
        if self._configuration_file_hash:
            flags.extend(["-configuration-file-hash", self._configuration_file_hash])
        if self._load_initial_state_from is not None:
            flags.extend(["-load-state-from", self._load_initial_state_from])
            if self._changed_files_path is not None:
                flags.extend(["-changed-files-path", self._changed_files_path])
        elif self._changed_files_path is not None:
            LOG.error(
                "--load-initial-state-from must be set if --changed-files-path is set."
            )
        flags.extend(
            [
                "-workers",
                str(self._number_of_workers),
                "-expected-binary-version",
                self._configuration.version_hash,
            ]
        )
        search_path = self._configuration.search_path + typeshed_search_path(
            self._configuration.typeshed
        )
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])

        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])

        extensions = self._configuration.extensions
        for extension in extensions:
            flags.extend(["-extension", extension])

        return flags

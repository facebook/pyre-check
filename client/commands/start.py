# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
from logging import Logger
from typing import List, Optional

from .. import (
    command_arguments,
    configuration_monitor,
    filesystem,
    project_files_monitor,
)
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .command import IncrementalStyle
from .reporting import Reporting


LOG: Logger = logging.getLogger(__name__)


class Start(Reporting):
    NAME = "start"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        terminal: bool,
        store_type_check_resolution: bool,
        use_watchman: bool,
        incremental_style: IncrementalStyle,
    ) -> None:
        super(Start, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._terminal = terminal
        self._store_type_check_resolution = store_type_check_resolution
        self._use_watchman = use_watchman
        self._incremental_style = incremental_style

        self._enable_logging_section("environment")

    def _start_configuration_monitor(self) -> None:
        if self._use_watchman:
            configuration_monitor.ConfigurationMonitor(
                self._command_arguments,
                self._configuration,
                self._analysis_directory,
                self._configuration.project_root,
                self._original_directory,
                self._configuration.local_root,
                list(self._configuration.other_critical_files),
            ).daemonize()

    def _run(self) -> None:
        lock = os.path.join(self._configuration.log_directory, "client.lock")
        LOG.info("Waiting on the pyre client lock.")
        with filesystem.acquire_lock(lock, blocking=True):
            self._start_configuration_monitor()
            # This unsafe call is OK due to the client lock always
            # being acquired before starting a server - no server can
            # spawn in the interim which would cause a race.
            try:
                with filesystem.acquire_lock(
                    os.path.join(
                        self._configuration.log_directory, "server", "server.lock"
                    ),
                    blocking=False,
                ):
                    pass
            except OSError:
                LOG.warning(
                    "Server at `%s` exists, skipping.",
                    self._analysis_directory.get_root(),
                )
                return

            self._analysis_directory.prepare()

            self._call_client(command=self.NAME).check()

            if self._use_watchman:
                try:
                    file_monitor = project_files_monitor.ProjectFilesMonitor(
                        self._configuration,
                        self._configuration.project_root,
                        self._analysis_directory,
                    )
                    file_monitor.daemonize()
                    LOG.debug("Initialized file monitor.")
                except project_files_monitor.MonitorException as error:
                    LOG.warning("Failed to initialize file monitor: %s", error)

    def _flags(self) -> List[str]:
        flags = super()._flags()
        if self._taint_models_path:
            for path in self._taint_models_path:
                flags.extend(["-taint-models", path])
        filter_directories = self._get_directories_to_analyze()
        filter_directories.update(
            set(self._configuration.get_existent_do_not_ignore_errors_in_paths())
        )
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])

        ignore_all_errors_paths = (
            self._configuration.get_existent_ignore_all_errors_paths()
        )
        if len(ignore_all_errors_paths):
            flags.extend(
                ["-ignore-all-errors", ";".join(sorted(ignore_all_errors_paths))]
            )
        if self._terminal:
            flags.append("-terminal")
        if self._store_type_check_resolution:
            flags.append("-store-type-check-resolution")
        if not self._command_arguments.no_saved_state:
            save_initial_state_to = self._command_arguments.save_initial_state_to
            if save_initial_state_to and os.path.isdir(
                os.path.dirname(save_initial_state_to)
            ):
                flags.extend(["-save-initial-state-to", save_initial_state_to])
            saved_state_project = self._command_arguments.saved_state_project
            if saved_state_project:
                flags.extend(["-saved-state-project", saved_state_project])
                relative_local_root = self._configuration.relative_local_root
                if relative_local_root is not None:
                    flags.extend(
                        ["-saved-state-metadata", relative_local_root.replace("/", "$")]
                    )
            configuration_file_hash = self._configuration.file_hash
            if configuration_file_hash:
                flags.extend(["-configuration-file-hash", configuration_file_hash])
            load_initial_state_from = self._command_arguments.load_initial_state_from
            changed_files_path = self._command_arguments.changed_files_path
            if load_initial_state_from is not None:
                flags.extend(["-load-state-from", load_initial_state_from])
                if changed_files_path is not None:
                    flags.extend(["-changed-files-path", changed_files_path])
            elif changed_files_path is not None:
                LOG.error(
                    "--load-initial-state-from must be set "
                    + "if --changed-files-path is set."
                )
        flags.extend(
            [
                "-workers",
                str(self._configuration.get_number_of_workers()),
                "-expected-binary-version",
                self._configuration.get_version_hash_respecting_override()
                or "unversioned",
            ]
        )

        search_path = [
            search_path.command_line_argument()
            for search_path in self._configuration.get_existent_search_paths()
        ]
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])

        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])

        extensions = [
            extension.command_line_argument()
            for extension in self._configuration.extensions
        ]
        for extension in extensions:
            flags.extend(["-extension", extension])

        if self._incremental_style != IncrementalStyle.SHALLOW:
            flags.append("-new-incremental-check")

        if self._configuration.autocomplete:
            flags.append("-autocomplete")
        flags.extend(self._feature_flags())

        return flags

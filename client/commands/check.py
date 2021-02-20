# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from logging import Logger
from typing import List, Optional

from .. import command_arguments
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .command import ExitCode
from .reporting import Reporting


LOG: Logger = logging.getLogger(__name__)


class Check(Reporting):
    NAME = "check"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Check, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._command_arguments.source_directories,
            self._command_arguments.targets,
            self._configuration,
            self._original_directory,
            filter_directory=self._command_arguments.filter_directory,
            isolate=True,
        )

    def _flags(self) -> List[str]:
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        filter_directories.update(
            set(self._configuration.get_existent_do_not_ignore_errors_in_paths())
        )
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])
        flags.extend(["-workers", str(self._configuration.get_number_of_workers())])

        search_path = [
            search_path.command_line_argument()
            for search_path in self._configuration.get_existent_search_paths()
        ]

        ignore_all_errors_paths = (
            self._configuration.get_existent_ignore_all_errors_paths()
        )
        if len(ignore_all_errors_paths):
            flags.extend(
                ["-ignore-all-errors", ";".join(sorted(ignore_all_errors_paths))]
            )
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
        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()

        result = self._call_client(command="check")
        errors = self._get_errors(result)
        self._print(errors)

        if errors:
            self._exit_code = ExitCode.FOUND_ERRORS

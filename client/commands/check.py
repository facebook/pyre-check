# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from logging import Logger
from typing import List, Optional

from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .command import ExitCode, typeshed_search_path
from .reporting import Reporting


LOG: Logger = logging.getLogger(__name__)


class Check(Reporting):
    NAME = "check"

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Check, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        check = parser.add_parser(
            cls.NAME,
            epilog="""
          Runs a one-time check of a project without initializing a type check server.
        """,
        )
        check.set_defaults(command=cls)

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._arguments,
            self._configuration,
            self._original_directory,
            self._current_directory,
            build=True,
            isolate=True,
        )

    def _flags(self) -> List[str]:
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])
        flags.extend(["-workers", str(self._number_of_workers)])
        search_path = self._configuration.search_path + typeshed_search_path(
            self._configuration.typeshed
        )
        if len(self._configuration.ignore_all_errors):
            flags.extend(
                [
                    "-ignore-all-errors",
                    ";".join(sorted(self._configuration.ignore_all_errors)),
                ]
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

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()

        result = self._call_client(command="check")
        errors = self._get_errors(result)
        self._print(errors)

        if errors:
            self._exit_code = ExitCode.FOUND_ERRORS

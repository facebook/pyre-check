# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import json
import logging
import os
from typing import Any, Dict, List, Optional, Sequence, Set

from .. import command_arguments
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..error import LegacyError, print_errors
from .command import ClientException, Command, Result


LOG: logging.Logger = logging.getLogger(__name__)


class Reporting(Command):
    NAME = "reporting"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super().__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )

    def _print(self, errors: Sequence[LegacyError]) -> None:
        print_errors(errors, output=self._output)

    def _get_directories_to_analyze(self) -> Set[str]:
        return self._analysis_directory.get_filter_roots()

    @staticmethod
    def _load_errors_from_json(json_output: str) -> List[Dict[str, Any]]:
        try:
            json_dictionary = json.loads(json_output)
        except (json.JSONDecodeError):
            raise ClientException(f"Invalid JSON output: `{json_output}`.")

        error_list = []
        if isinstance(json_dictionary, dict) and "errors" in json_dictionary:
            error_list = json_dictionary["errors"]
        else:
            # TODO(T62259082): Identify why the client receives such JSON output.
            LOG.error(
                "Received invalid JSON output for the last `pyre` command"
                + " (known bug: GitHub issue #238 / T62259082)."
                + " You may want to rerun your command."
            )
            LOG.debug("Invalid JSON output: %s", json_output)
        return error_list

    def _parse_raw_errors(self, result: Result) -> Sequence[LegacyError]:
        result.check()
        errors: List[LegacyError] = []
        results: List[Dict[str, Any]] = self._load_errors_from_json(result.output)
        for error in results:
            errors.append(LegacyError.create(error, ignore_error=False))
        return errors

    def _relativize_errors(
        self, relative_root: str, errors: Sequence[LegacyError]
    ) -> Sequence[LegacyError]:
        relativized_errors = []
        for error in errors:
            path = os.path.realpath(os.path.join(relative_root, error.error.path))
            # Nonexistent paths can be created when search path stubs are renamed.
            if not path.startswith(
                self._configuration.project_root
            ) or not os.path.exists(path):
                continue

            # Relativize path to user's cwd.
            relative_path = self._relative_path(path)
            relativized_errors.append(error.with_path(relative_path))

        return relativized_errors

    def _filter_errors(self, errors: Sequence[LegacyError]) -> Sequence[LegacyError]:
        filtered_errors = [error for error in errors if not error.is_ignored()]
        sorted_errors = sorted(
            filtered_errors,
            key=lambda error: (error.error.path, error.error.line, error.error.column),
        )
        return sorted_errors

    def _get_errors(self, result: Result) -> Sequence[LegacyError]:
        analysis_root = os.path.realpath(
            self._analysis_directory.get_root_path().get_root()
        )
        errors = self._relativize_errors(analysis_root, self._parse_raw_errors(result))

        return self._filter_errors(errors)

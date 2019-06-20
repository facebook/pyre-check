# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import fnmatch
import json
import logging
import os
from typing import Any, Dict, Iterable, List, Sequence, Set  # noqa

from .. import log
from ..configuration import Configuration
from ..error import Error
from ..filesystem import AnalysisDirectory, translate_path
from .command import ClientException, Command, Result


LOG = logging.getLogger(__name__)  # type: logging.Logger

TEXT = "text"  # type: str
JSON = "json"  # type: str


class Reporting(Command):
    def __init__(
        self,
        arguments: argparse.Namespace,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
    ) -> None:
        super().__init__(arguments, configuration, analysis_directory)
        self._verbose = arguments.verbose  # type: bool
        self._output = arguments.output  # type: str
        self._ignore_all_errors_paths = (
            configuration.ignore_all_errors
        )  # type: Iterable[str]

    def _print(self, errors: Sequence[Error]) -> None:
        if errors:
            length = len(errors)
            LOG.error("Found %d type error%s!", length, "s" if length > 1 else "")
        else:
            LOG.log(log.SUCCESS, "No type errors found")

        if self._output == TEXT:
            log.stdout.write("\n".join([repr(error) for error in errors]))
        else:
            log.stdout.write(json.dumps([error.__dict__ for error in errors]))

    def _get_directories_to_analyze(self) -> Set[str]:
        current_project_directories = self._analysis_directory.get_filter_root()
        # The server may not exist in the same directory, so use absolute paths.
        directories_to_analyze = {
            translate_path(os.getcwd(), filter_root)
            for filter_root in current_project_directories
        }
        return directories_to_analyze

    def _get_errors(
        self, result: Result, bypass_filtering: bool = False
    ) -> Sequence[Error]:
        result.check()

        errors = []  # type: List[Error]
        # pyre-ignore: T39175181
        results = {}  # type: List[Dict[str, Any]]
        try:
            results = json.loads(result.output)
            # TODO(T39755668): deprecate 'if' condition eventually.
            if "errors" in results:
                # pyre-ignore: T39755668
                results = results["errors"]
        except (json.JSONDecodeError, ValueError):
            raise ClientException("Invalid output: `{}`.".format(result.output))

        for error in results:
            full_path = os.path.realpath(
                os.path.join(self._analysis_directory.get_root(), error["path"])
            )
            # Relativize path to user's cwd.
            relative_path = self._relative_path(full_path)
            error["path"] = relative_path
            ignore_error = False
            external_to_global_root = True
            if full_path.startswith(self._current_directory):
                external_to_global_root = False
            for absolute_ignore_path in self._ignore_all_errors_paths:
                if fnmatch.fnmatch(full_path, (absolute_ignore_path + "*")):
                    ignore_error = True
                    break
            errors.append(Error(ignore_error, external_to_global_root, **error))

        if bypass_filtering:
            return errors
        else:
            filtered_errors = [
                error
                for error in errors
                if (
                    not error.is_ignored()
                    and (self._verbose or not (error.is_external_to_global_root()))
                )
            ]
            sorted_errors = sorted(
                filtered_errors,
                key=lambda error: (error.path, error.line, error.column),
            )

            return sorted_errors

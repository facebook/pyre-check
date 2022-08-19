# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
import pathlib
import sys
import tempfile
from argparse import Namespace
from dataclasses import dataclass
from logging import Logger
from typing import Dict, Iterable, List, Tuple

from .run_server_integration_test import extract_typeshed

LOG: Logger = logging.getLogger(__name__)

def _fetch_commit_paths(repository_path: str) -> Iterable[pathlib.Path]:
    return sorted(pathlib.Path(repository_path).iterdir())


class Repository:
    def __init__(
        self,
        typeshed_zip_path: str,
        base_directory: str,
        repository_path: str,
        debug: bool
    ) -> None:
        self._commit_paths: Iterable[pathlib.Path] = _fetch_commit_paths(repository_path)
        self._pyre_directory: pathlib.Path = pathlib.Path(base_directory) / "repository"
        self.debug: bool = debug
        self._initialize_pyre_directory(self._pyre_directory, typeshed_zip_path, base_directory)
        pass

    @staticmethod
    def _initialize_pyre_directory(pyre_directory: pathlib.Path, typeshed_zip_path: str, base_directory: str) -> None:
        pathlib.Path(pyre_directory).mkdir()
        pyre_configuration_path = pathlib.Path(pyre_directory) / ".pyre_configuration"
        typeshed_location = extract_typeshed(typeshed_zip_path, base_directory)
        pyre_configuration_path.write_text(json.dumps({
                    "source_directories": ["."],
                    "typeshed": typeshed_location,
                    "search_path": ["stubs"],
                }))

        watchman_configuration_path = pathlib.Path(pyre_directory) / ".watchmanconfig"
        watchman_configuration_path.write_text(json.dumps({}))

    def get_repository_directory(self) -> pathlib.Path:
        return self._pyre_directory

    def get_commit_paths(self) -> Iterable[pathlib.Path]:
        return self._commit_paths

    def overlay_update(self, file_path: pathlib.Path) -> str:
        """The method will load the file from the original Pyre source directory and copy it to the temporary pyre directory"""
        return '["TypeErrors", []]'

    def incremental_update(self, file_path: pathlib.Path) -> str:
        """ Performs a call to Pyre to perform an incremental update with only a single file_path (specified by file_path)"""
        return '["TypeErrors", []]'


    def modify_file(self, file_path: pathlib.Path) -> None:
        """ For a given file_path, replace the contents of that file_path with the contents in the original pyre base directory"""
        ...

    def initiate_empty_files(self, file_list: List[pathlib.Path]) -> None:
        ...

    def run_pyre(self, command: str, *arguments: str) -> str:
        return ""

@dataclass(frozen=True)
class FileErrorsResult:
    file_name: pathlib.Path
    overlay_errors: str
    incremental_errors: str

def _get_file_errors_result(repository: Repository, file_path: pathlib.Path) -> FileErrorsResult:
    overlay_errors_response = repository.overlay_update(file_path)
    repository.modify_file(file_path)
    incremental_errors_response = repository.incremental_update(file_path)
    commit_result = FileErrorsResult(file_path, overlay_errors_response, incremental_errors_response)
    return commit_result


def run_unsaved_changes_test(
    typeshed_zip_path: str, repository_path: str, debug: bool
) -> int:

    with tempfile.TemporaryDirectory() as base_directory:
        repository = Repository(
            typeshed_zip_path, base_directory, repository_path, debug
        )
        try:
            repository.run_pyre(
                "--logging-sections",
                "-n",
                "start",
            )
            result = 0
            for commit in repository.get_commit_paths():
                discrepancies: Dict[pathlib.Path, Tuple[str, str]] = {}
                python_file_list = list(commit.glob("*.py"))
                repository.initiate_empty_files(python_file_list)
                for file_path in python_file_list:
                    commit_result = _get_file_errors_result(repository, file_path)
                    if commit_result.overlay_errors != commit_result.incremental_errors:
                        discrepancies[file_path] = (commit_result.overlay_errors, commit_result.incremental_errors)
                        LOG.error("Found discrepancies in %s for incremental check, file: %s", commit, file_path)
                result = _print_discrepancies(discrepancies, commit.name)
                if result:
                    break
            repository.run_pyre("stop")
            return result
        except Exception as uncaught_pyre_exception:
            LOG.error("Uncaught exception: `%s`", str(uncaught_pyre_exception))
            LOG.info("Pyre rage: %s", repository.run_pyre("rage"))
            raise uncaught_pyre_exception

def _print_discrepancies(discrepancies: Dict[pathlib.Path, Tuple[str, str]], commit: str) -> int:
    if len(discrepancies) == 0:
        return 0
    for file_name, (actual_error, expected_error) in discrepancies.items():
        LOG.error(
            "Difference found for revision: {}, file_name: {}\n".format(commit, file_name),
        )
        LOG.error(
            "Actual errors (pyre overlayUpdate): {}\n".format(actual_error),
        )
        LOG.error(
            "Expected errors (pyre incremental): {}\n".format(expected_error),
        )
    return 1

def run(repository_location: str, typeshed_zip_path: str, debug: bool) -> int:
    typeshed_zip_path = typeshed_zip_path
    return run_unsaved_changes_test(
        typeshed_zip_path, repository_location, debug
    )

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "repository_location", help="Path to directory with fake commit list"
    )
    parser.add_argument(
        "--typeshed-zip-path",
        help="Path to zip containing typeshed.",
    )
    parser.add_argument("--debug", action="store_true", default=False)
    arguments: Namespace = parser.parse_args()
    sys.exit(
        run(arguments.repository_location, arguments.typeshed_zip_path, arguments.debug)
    )

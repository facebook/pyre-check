#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

r"""
Run an integration test of the pyre server's incremental update logic.

You can run this from the pyre-check repository root as follows:
```
python3 scripts/run_server_integration_test.py \
    --typeshed-zip-path stubs/typeshed/typeshed.zip \
    source/command/test/integration/fake_repository/
```
"""

# pyre-unsafe

import argparse
import filecmp
import json
import logging
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
from argparse import Namespace
from contextlib import contextmanager
from logging import Logger
from typing import Generator, Optional, Tuple
from zipfile import ZipFile


LOG: Logger = logging.getLogger(__name__)


def is_readable_directory(directory: str) -> bool:
    return os.path.isdir(directory) and os.access(directory, os.R_OK)


def assert_readable_directory(directory: str) -> None:
    if not os.path.isdir(directory):
        raise Exception("{} is not a valid directory.".format(directory))
    if not os.access(directory, os.R_OK):
        raise Exception("{} is not a readable directory.".format(directory))


def extract_typeshed(typeshed_zip_path: str, base_directory: str) -> str:
    typeshed = os.path.join(base_directory, "typeshed-master")
    os.mkdir(typeshed)
    with ZipFile(typeshed_zip_path, "r") as typeshed_zip:
        typeshed_zip.extractall(base_directory)
    assert_readable_directory(typeshed)
    # Prune all non-essential directories.
    for entry in os.listdir(typeshed):
        if entry in ["stdlib", "third_party", "stubs"]:
            continue
        full_path = os.path.join(typeshed, entry)
        if os.path.isfile(full_path):
            os.remove(full_path)
        elif os.path.isdir(full_path):
            shutil.rmtree(full_path)
    return typeshed


def poor_mans_rsync(source_directory: str, destination_directory: str) -> None:
    ignored_files = [".pyre_configuration", ".watchmanconfig"]
    ignored_directories = [".pyre"]
    # Do not delete the server directory while copying!
    assert_readable_directory(source_directory)
    source_files = [
        entry
        for entry in os.listdir(source_directory)
        if entry not in ignored_files
        and os.path.isfile(os.path.join(source_directory, entry))
    ]
    assert_readable_directory(destination_directory)
    destination_files = [
        entry
        for entry in os.listdir(destination_directory)
        if entry not in ignored_files
        and os.path.isfile(os.path.join(destination_directory, entry))
    ]
    source_directories = [
        entry
        for entry in os.listdir(source_directory)
        if os.path.isdir(os.path.join(source_directory, entry))
        and entry not in ignored_directories
    ]
    destination_directories = [
        entry
        for entry in os.listdir(destination_directory)
        if os.path.isdir(os.path.join(destination_directory, entry))
        and entry not in ignored_directories
    ]

    # Copy all directories over blindly.
    for directory in source_directories:
        source = os.path.join(source_directory, directory)
        destination = os.path.join(destination_directory, directory)
        if os.path.isdir(destination):
            shutil.rmtree(destination)
        shutil.copytree(source, destination)

    # Delete any missing directories.
    for directory in destination_directories:
        if directory not in source_directories:
            destination = os.path.join(destination_directory, directory)
            shutil.rmtree(destination)

    for filename in destination_files:
        if filename not in source_files:
            os.remove(os.path.join(destination_directory, filename))

    # Compare files across source and destination.
    (match, mismatch, error) = filecmp.cmpfiles(
        source_directory, destination_directory, source_files, shallow=False
    )
    for filename in mismatch:
        shutil.copy2(os.path.join(source_directory, filename), destination_directory)
    for filename in error:
        shutil.copy2(os.path.join(source_directory, filename), destination_directory)


class Repository:
    def __init__(
        self,
        typeshed_zip_path: str,
        base_directory: str,
        repository_path: str,
        debug: bool,
    ) -> None:

        # Parse list of fake commits.
        assert_readable_directory(repository_path)
        self._base_repository_path = os.path.realpath(repository_path)
        commits_list = os.listdir(self._base_repository_path)
        list.sort(commits_list)
        for commit in commits_list:
            assert_readable_directory(os.path.join(self._base_repository_path, commit))
        self._commits_list = iter(commits_list)

        # Move into the temporary repository directory.
        self._pyre_directory = os.path.join(base_directory, "repository")
        os.mkdir(self._pyre_directory)
        os.chdir(self._pyre_directory)

        with open(
            os.path.join(self._pyre_directory, ".pyre_configuration"), "w"
        ) as configuration_file:
            typeshed_location = extract_typeshed(typeshed_zip_path, base_directory)
            json.dump(
                {
                    "source_directories": ["."],
                    "typeshed": typeshed_location,
                    "search_path": ["stubs"],
                },
                configuration_file,
            )
        with open(
            os.path.join(self._pyre_directory, ".watchmanconfig"), "w"
        ) as watchman_configuration:
            json.dump({}, watchman_configuration)

        self.debug = debug
        # Seed the repository with the base commit.
        self.__next__()

    def get_repository_directory(self) -> str:
        return self._pyre_directory

    def __iter__(self) -> "Repository":
        return self

    def __next__(self) -> str:
        self._current_commit = self._commits_list.__next__()
        LOG.info("Moving to commit named: %s" % self._current_commit)

        # Last empty path is needed to terminate the path with a directory separator.
        original_path = os.path.join(
            self._base_repository_path, self._current_commit, ""
        )

        self._copy_commit(original_path, ".")

        return self._current_commit

    def _copy_commit(self, original_path: str, destination_path: str) -> None:
        """
        Copies the next commit at original_path to destination path. Can be
        overridden by child classes to change copying logic.
        """
        # I could not find the right flags for rsync to touch/write
        # only the changed files. This is crucial for watchman to
        # generate the right notifications. Hence, this.
        poor_mans_rsync(original_path, destination_path)

    def get_pyre_errors(self) -> Tuple[str, str]:
        # Run the full check first so that watchman updates have time to propagate.
        check_errors = self.run_pyre("check")
        incremental_errors = self.run_pyre("incremental", "--no-start")
        return (incremental_errors, check_errors)

    def run_pyre(self, command: str, *arguments: str) -> str:
        pyre_client = os.getenv("PYRE_TEST_CLIENT_LOCATION", "pyre")
        standard_error = None if self.debug else subprocess.DEVNULL
        try:
            output = subprocess.check_output(
                [pyre_client, "--noninteractive", "--output=json", command, *arguments],
                stderr=standard_error,
            )
        except subprocess.CalledProcessError as error:
            if error.returncode not in [0, 1]:
                raise error
            output = error.output
        return output.decode("utf-8")


def run_integration_test(
    typeshed_zip_path: str, repository_path: str, debug: bool
) -> int:
    if not shutil.which("watchman"):
        LOG.error("The integration test cannot work if watchman is not installed!")
        return 1

    with tempfile.TemporaryDirectory() as base_directory:
        discrepancies = {}
        repository = Repository(
            typeshed_zip_path, base_directory, repository_path, debug
        )
        with _watch_directory(repository.get_repository_directory()):
            try:
                repository.run_pyre(
                    "--logging-sections",
                    "server",
                    "start",
                )
                for commit in repository:
                    (actual_error, expected_error) = repository.get_pyre_errors()
                    if actual_error != expected_error:
                        discrepancies[commit] = (actual_error, expected_error)
                        LOG.error("Found discrepancies in %s", commit)
                        if debug:
                            break
                repository.run_pyre("stop")
            except Exception as uncaught_pyre_exception:
                LOG.error("Uncaught exception: `%s`", str(uncaught_pyre_exception))
                LOG.info("Pyre rage: %s", repository.run_pyre("rage"))
                raise uncaught_pyre_exception

        if discrepancies:
            LOG.error("Pyre rage:")
            print(repository.run_pyre("rage"), file=sys.stderr)
            LOG.error("Found discrepancies between incremental and complete checks!")
            for revision, (actual_error, expected_error) in discrepancies.items():
                print("Difference found for revision: {}".format(revision))
                print("Actual errors (pyre incremental): {}".format(actual_error))
                print("Expected errors (pyre check): {}".format(expected_error))
            return 1

    return 0


# In general, saved state load/saves are a distributed system problem - the file systems
# are completely different. Make sure that Pyre doesn't rely on absolute paths when
# loading via this test.
def run_saved_state_test(typeshed_zip_path: str, repository_path: str) -> int:
    # Copy files over to a temporary directory.
    original_directory = os.getcwd()
    saved_state_path = tempfile.NamedTemporaryFile().name
    with tempfile.TemporaryDirectory() as saved_state_create_directory:
        repository = Repository(
            typeshed_zip_path,
            saved_state_create_directory,
            repository_path,
            debug=False,
        )
        repository.run_pyre(
            "--save-initial-state-to", saved_state_path, "incremental", "--no-watchman"
        )
        repository.__next__()
        expected_errors = repository.run_pyre("check")
        repository.run_pyre("stop")

    os.chdir(original_directory)
    with tempfile.TemporaryDirectory() as saved_state_load_directory:
        repository = Repository(
            typeshed_zip_path, saved_state_load_directory, repository_path, debug=False
        )
        repository.__next__()

        changed_files = [
            path
            for path in pathlib.Path(repository.get_repository_directory()).iterdir()
            if path.suffix == ".py"
        ]
        changed_files_path = (
            pathlib.Path(saved_state_load_directory) / "changed_files.txt"
        )
        changed_files_path.write_text("\n".join([str(path) for path in changed_files]))

        repository.run_pyre(
            "--load-initial-state-from",
            saved_state_path,
            "--changed-files-path",
            str(changed_files_path),
            "start",
            "--no-watchman",
        )
        actual_errors = repository.run_pyre("incremental")
        repository.run_pyre("stop")

    if actual_errors != expected_errors:
        LOG.error("Actual errors are not equal to expected errors.")
        print("Actual errors (pyre incremental): {}".format(actual_errors))
        print("Expected errors (pyre check): {}".format(expected_errors))
        return 1
    return 0


@contextmanager
def _watch_directory(source_directory) -> Generator[None, None, None]:
    subprocess.check_call(
        ["watchman", "watch", source_directory],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    yield
    subprocess.check_call(
        ["watchman", "watch-del", source_directory],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def run(repository_location: str, typeshed_zip_path: Optional[str], debug: bool) -> int:
    retries = 3
    typeshed_zip_path = typeshed_zip_path or str(
        pathlib.Path.cwd() / "stubs/typeshed/typeshed.zip"
    )
    original_directory: str = os.getcwd()
    while retries > 0:
        try:
            os.chdir(original_directory)
            exit_code = run_integration_test(
                typeshed_zip_path, repository_location, debug
            )
            if exit_code != 0:
                sys.exit(exit_code)
            print("### Running Saved State Test ###")
            os.chdir(original_directory)
            return run_saved_state_test(typeshed_zip_path, repository_location)
        except Exception as e:
            LOG.error("Exception raised in integration test:\n %s \nretrying...", e)
            # Retry the integration test for uncaught exceptions. Caught issues will
            # result in an exit code of 1.
            retries = retries - 1
    return 1


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO, format=" >>> %(asctime)s %(levelname)s %(message)s"
    )
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "repository_location", help="Path to directory with fake commit list"
    )
    parser.add_argument(
        "--typeshed-zip-path",
        help="Path to zip containing typeshed.",
        type=os.path.abspath,
    )
    parser.add_argument("--debug", action="store_true", default=False)
    arguments: Namespace = parser.parse_args()
    sys.exit(
        run(arguments.repository_location, arguments.typeshed_zip_path, arguments.debug)
    )

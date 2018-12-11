#!/usr/bin/env python3

import argparse
import filecmp
import fileinput
import json
import logging
import os
import shutil
import subprocess
import sys
import tempfile
from contextlib import contextmanager


LOG = logging.getLogger(__name__)


def is_readable_directory(directory: str) -> bool:
    return os.path.isdir(directory) and os.access(directory, os.R_OK)


def assert_readable_directory(directory: str) -> None:
    if not os.path.isdir(directory):
        raise Exception("{} is not a valid directory.".format(directory))
    if not os.access(directory, os.R_OK):
        raise Exception("{} is not a readable directory.".format(directory))


def extract_typeshed(configuration_file: str):
    try:
        with open(configuration_file) as file:
            configuration = json.load(file)

            typeshed = configuration.get("typeshed")
            version_hash = configuration.get("version")
            if not typeshed:
                return None
            if version_hash:
                typeshed = typeshed.replace("%V", version_hash)
            return typeshed
    except Exception as e:
        LOG.error("Exception raised while reading %s:", configuration_file)
        LOG.error("%s", e)
    return None


def get_typeshed_from_github(base_directory: str):
    typeshed = os.path.join(base_directory, "typeshed")
    os.mkdir(typeshed)

    result = subprocess.run(
        ["git", "clone", "https://github.com/python/typeshed.git", typeshed]
    )
    if result.returncode != 0:
        return None
    assert_readable_directory(typeshed)
    # Prune all non-essential directories.
    for entry in os.listdir(typeshed):
        if entry in ["stdlib", "third_party"]:
            continue
        full_path = os.path.join(typeshed, entry)
        if os.path.isfile(full_path):
            os.remove(full_path)
        elif os.path.isdir(full_path):
            shutil.rmtree(full_path)
    return typeshed


def find_test_typeshed(base_directory: str) -> str:
    test_typeshed = os.getenv("PYRE_TEST_TYPESHED_LOCATION")
    if test_typeshed and is_readable_directory(test_typeshed):
        LOG.info("Using typeshed from environment: %s", test_typeshed)
        return test_typeshed

    # Check if we can infer typeshed from a .pyre_configuration
    # file living in a directory above.
    path = os.getcwd()
    while True:
        configuration = os.path.join(path, ".pyre_configuration")
        if os.path.isfile(configuration):
            test_typeshed = extract_typeshed(configuration)
            if test_typeshed and is_readable_directory(test_typeshed):
                LOG.info("Using typeshed from configuration: %s", test_typeshed)
                return test_typeshed
        parent_directory = os.path.dirname(path)
        if parent_directory == path:
            # We have reached the root.
            break
        path = parent_directory

    # Try and fetch it from the web in a temporary directory.
    temporary_typeshed = get_typeshed_from_github(base_directory)
    if temporary_typeshed and is_readable_directory(temporary_typeshed):
        LOG.info("Using typeshed from the web: %s", temporary_typeshed)
        return temporary_typeshed
    raise Exception("Could not find a valid typeshed to use")


def poor_mans_rsync(source_directory, destination_directory):
    # Do not delete the server directory while copying!
    assert_readable_directory(source_directory)
    source_files = [
        entry
        for entry in os.listdir(source_directory)
        if os.path.isfile(os.path.join(source_directory, entry))
    ]
    assert_readable_directory(destination_directory)
    destination_files = [
        entry
        for entry in os.listdir(destination_directory)
        if os.path.isfile(os.path.join(destination_directory, entry))
    ]
    source_directories = [
        entry
        for entry in os.listdir(source_directory)
        if os.path.isdir(os.path.join(source_directory, entry))
    ]
    # Copy all directories over blindly.
    for directory in source_directories:
        source = os.path.join(source_directory, directory)
        destination = os.path.join(destination_directory, directory)
        if os.path.isdir(destination):
            shutil.rmtree(destination)
        shutil.copytree(source, destination)

    for filename in destination_files:
        if filename not in source_files:
            LOG.info("Removing file '%s' from destination" % filename)
            os.remove(os.path.join(destination_directory, filename))

    # Compare files across source and destination.
    (match, mismatch, error) = filecmp.cmpfiles(
        source_directory, destination_directory, source_files
    )
    for filename in match:
        LOG.info("Skipping file '%s' because it matches" % filename)
    for filename in mismatch:
        LOG.info("Copying file '%s' due to mismatch" % filename)
        shutil.copy2(os.path.join(source_directory, filename), destination_directory)
    for filename in error:
        LOG.info("Copying file '%s' because it is missing" % filename)
        shutil.copy2(os.path.join(source_directory, filename), destination_directory)


class Repository:
    def __init__(self, base_directory: str, repository_path: str) -> None:
        self._test_typeshed_location = find_test_typeshed(base_directory)

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

        # Seed the repository with the base commit.
        self.__next__()

    def get_repository_directory(self) -> str:
        return self._pyre_directory

    def __iter__(self):
        return self

    def __next__(self):
        self._current_commit = self._commits_list.__next__()
        LOG.info("Moving to commit named: %s" % self._current_commit)

        # Last empty path is needed to terminate the path with a directory separator.
        original_path = os.path.join(
            self._base_repository_path, self._current_commit, ""
        )
        # I could not find the right flags for rsync to touch/write
        # only the changed files. This is crucial for watchman to
        # generate the right notifications. Hence, this.
        poor_mans_rsync(original_path, ".")

        self._resolve_typeshed_location(".pyre_configuration")
        return self._current_commit

    def _resolve_typeshed_location(self, filename):
        with fileinput.input(filename, inplace=True) as f:
            for line in f:
                print(
                    line.replace(
                        "PYRE_TEST_TYPESHED_LOCATION", self._test_typeshed_location
                    ),
                    end="",
                )

    def get_pyre_errors(self):
        incremental_errors = self.run_pyre("incremental")
        check_errors = self.run_pyre("check")
        return (incremental_errors, check_errors)

    def run_pyre(self, command: str) -> str:
        pyre_client = os.getenv("PYRE_TEST_CLIENT_LOCATION", "pyre")
        try:
            output = subprocess.check_output(
                [
                    pyre_client,
                    "--noninteractive",
                    "--show-parse-errors",
                    "--output=json",
                    command,
                ]
            )
        except subprocess.CalledProcessError as error:
            if error.returncode not in [0, 1]:
                raise error
            output = error.output
        return output.decode("utf-8")


def run_integration_test(repository_path) -> int:
    with tempfile.TemporaryDirectory() as base_directory:
        discrepancies = {}
        repository = Repository(base_directory, repository_path)
        with _watch_directory(repository.get_repository_directory()):
            repository.run_pyre("start")
            for commit in repository:
                (actual_error, expected_error) = repository.get_pyre_errors()
                if actual_error != expected_error:
                    discrepancies[commit] = (actual_error, expected_error)
            repository.run_pyre("stop")

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


@contextmanager
def _watch_directory(source_directory):
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


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO, format=" >>> %(asctime)s %(levelname)s %(message)s"
    )
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "repository_location", help="Path to directory with fake commit list"
    )
    arguments = parser.parse_args()
    sys.exit(run_integration_test(arguments.repository_location))

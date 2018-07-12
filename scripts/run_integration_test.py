#!/usr/bin/env python3

import argparse
import filecmp
import logging
import os
import shutil
import subprocess
import sys
import tempfile
from contextlib import contextmanager


LOG = logging.getLogger(__name__)


def assert_readable_directory(directory: str) -> None:
    if not os.path.isdir(directory):
        raise Exception("{} is not a valid directory.".format(directory))
    if not os.access(directory, os.R_OK):
        raise Exception("{} is not a readable directory.".format(directory))


class Repository:
    def __init__(self, pyre_directory, repository_path: str) -> None:
        # Parse list of fake commits.
        assert_readable_directory(repository_path)
        self._base_repository_path = os.path.realpath(repository_path)
        commits_list = os.listdir(self._base_repository_path)
        list.sort(commits_list)
        for commit in commits_list:
            assert_readable_directory(os.path.join(self._base_repository_path, commit))
        self._commits_list = iter(commits_list)

        # Move into the temporary directory.
        self._pyre_directory = pyre_directory
        os.chdir(pyre_directory)

        # Seed the repository with the base commit.
        self.__next__()

    def __iter__(self):
        return self

    def __next__(self):
        self._current_commit = self._commits_list.__next__()
        print(" >>> Moving to commit named: %s" % self._current_commit, file=sys.stderr)

        # Last empty path is needed to terminate the path with a directory separator.
        original_path = os.path.join(
            self._base_repository_path, self._current_commit, ""
        )
        # I could not find the right flags for rsync to touch/write
        # only the changed files. This is crucial for watchman to
        # generate the right notifications. Hence, this.
        self._poor_mans_rsync(original_path, ".")
        return self._current_commit

    def _poor_mans_rsync(self, source_directory, destination_directory):
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

        # First remove all destination files that are missing in the source.
        for filename in destination_files:
            if filename not in source_files:
                print(
                    " > Removing file '%s' from destination" % filename, file=sys.stderr
                )
                os.remove(os.path.join(destination_directory, filename))

        # Compare files across source and destination.
        (match, mismatch, error) = filecmp.cmpfiles(
            source_directory, destination_directory, source_files
        )
        for filename in match:
            print(
                " > Skipping file '%s' because it matches" % filename, file=sys.stderr
            )
        for filename in mismatch:
            print(" > Copying file '%s' due to mismatch" % filename, file=sys.stderr)
            shutil.copy2(
                os.path.join(source_directory, filename), destination_directory
            )
        for filename in error:
            print(
                " > Copying file '%s' because it is missing" % filename, file=sys.stderr
            )
            shutil.copy2(
                os.path.join(source_directory, filename), destination_directory
            )

    def get_pyre_errors(self):
        incremental_errors = self.run_pyre("incremental")
        check_errors = self.run_pyre("check")
        return (incremental_errors, check_errors)

    def run_pyre(self, command: str) -> str:
        try:
            output = subprocess.check_output(
                ["pyre", "--noninteractive", "--output=json", command]
            )
        except subprocess.CalledProcessError as error:
            output = error.output
        return output.decode("utf-8")


def run_integration_test(repository_path) -> int:
    base_directory = tempfile.mkdtemp()
    discrepancies = {}
    repository = Repository(base_directory, repository_path)
    with _watch_directory(base_directory):
        repository.run_pyre("start")
        for commit in repository:
            discrepancies[commit] = repository.get_pyre_errors()
        repository.run_pyre("stop")

    for revision, (actual_error, expected_error) in discrepancies.items():
        if actual_error != expected_error:
            LOG.error("Found discrepancies between incremental and complete checks!")
            print("Difference found for revision {}".format(revision))
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
        level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s"
    )
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "repository_location", help="Path to directory with fake commit list"
    )
    arguments = parser.parse_args()
    sys.exit(run_integration_test(arguments.repository_location))

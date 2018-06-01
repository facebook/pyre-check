#!/usr/bin/env python3

import argparse
import logging
import os
import subprocess
import sys
import tarfile
import tempfile
from contextlib import contextmanager


LOG = logging.getLogger(__name__)


class AtLatestRevision(Exception):
    pass


class Repository:
    def __init__(self, pyre_directory, repository_tar_path: str) -> None:
        with tarfile.open(repository_tar_path) as repository_tar:
            repository_tar.extractall(pyre_directory)
        self.pyre_directory = pyre_directory
        os.chdir(pyre_directory)
        self.latest_hash = subprocess.check_output(
            ["hg", "tip", "--template", "{node}"]
        ).decode("utf-8")

    def current_hash(self) -> str:
        return subprocess.check_output(
            ["hg", "log", "--limit", "1", "--template", "{node}"]
        ).decode("utf-8")

    def next(self) -> None:
        try:
            subprocess.check_call(["hg", "next"])
        except subprocess.CalledProcessError:
            raise AtLatestRevision

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


def run_integration_test(repository_tar_path) -> int:
    base_directory = tempfile.mkdtemp()
    discrepancies = {}
    repository = Repository(base_directory, repository_tar_path)
    with _watch_directory(base_directory):
        repository.run_pyre("start")
        try:
            while True:
                discrepancies[repository.current_hash()] = repository.get_pyre_errors()
                repository.next()  # noqa # Flake8 dislikes the use of next()
        except AtLatestRevision:
            pass
        repository.run_pyre("stop")

    for revision, (actual_error, expected_error) in discrepancies.items():
        if actual_error != expected_error:
            LOG.error("Found discrepancies between incremental and complete checks!")
            print("Difference found for revision {}".format(revision))
            print("Actual errors: {}".format(actual_error))
            print("Expected errors: {}".format(expected_error))
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
    parser.add_argument("repository_location", help="Path to tarred repository")
    arguments = parser.parse_args()
    sys.exit(run_integration_test(arguments.repository_location))

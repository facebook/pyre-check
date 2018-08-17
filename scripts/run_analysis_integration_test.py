#!/usr/bin/env python3

import difflib
import logging
import os
import subprocess
import sys


LOG = logging.getLogger(__name__)


def run_pyre(source_directory, models_directory):
    try:
        output = subprocess.check_output(
            [
                "pyre",
                "--noninteractive",
                "--source-directory",
                source_directory,
                "analyze",
                "--taint-models-path",
                models_directory,
            ]
        )
    except subprocess.CalledProcessError as error:
        if error.returncode not in [0, 1]:
            raise error
        output = error.output
    return output.decode("utf-8")


def compare_output(actual_errors, expected_errors):
    diff = difflib.unified_diff(
        expected_errors.splitlines(keepends=True),
        actual_errors.splitlines(keepends=True),
    )
    return "".join(list(diff))


def run_analysis_integration_test(source_directory, models_directory):
    actual_errors = run_pyre(source_directory, models_directory)
    try:
        expected_errors_file = os.path.join(source_directory, "expect.json")
        with open(os.path.join(expected_errors_file)) as file:
            expected_errors = file.read()
            result = compare_output(actual_errors, expected_errors)
            if result:
                print("Ran {}: ERROR.".format(os.path.basename(source_directory)))
                print("Difference in output:\n{}".format(result))
                return 1
            print("Ran {}: OK.".format(os.path.basename(source_directory)))
            return 0
    except Exception as e:
        LOG.error("Exception raised while reading %s:", expected_errors_file)
        LOG.error("%s", e)
    return 2


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO, format=" >>> %(asctime)s %(levelname)s %(message)s"
    )
    base_directory = "command/test/analysis_integration_test"
    models_directory = os.path.join(base_directory, "models")
    repositories = ["across_files", "across_files_2"]
    for repository in repositories:
        source_directory = os.path.join(base_directory, repository)
        error_code = run_analysis_integration_test(source_directory, models_directory)
        if error_code:
            sys.exit(error_code)

    sys.exit(error_code)

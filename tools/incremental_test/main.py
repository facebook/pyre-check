# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import enum
import json
import logging
import sys
from argparse import Namespace
from pathlib import Path
from typing import List, Optional

from . import report
from .batch import run_batch_benchmark, run_batch_test, RunnerResult
from .environment import SubprocessEnvironment
from .specification import InvalidSpecificationException, Specification


LOG: logging.Logger = logging.getLogger(__name__)


def _setup_logging(verbosity: int) -> None:
    logging.basicConfig(
        level=max(3 - verbosity, 0) * 10, format="%(asctime)s %(levelname)s %(message)s"
    )


def _log_test_statistics(results: List[RunnerResult]) -> None:
    results_with_output = [
        result for result in results if result.get_status() != "exception"
    ]
    LOG.warning(f"Total number of tests = {len(results)}")
    LOG.warning(f"Successfully finished tests = {len(results_with_output)}")
    LOG.warning(
        f"Tests finished with exceptions: {len(results) - len(results_with_output)}"
    )

    passed_results = [
        result for result in results_with_output if result.get_status() == "pass"
    ]
    LOG.warning(f"Tests passed = {len(passed_results)}")
    LOG.warning(f"Tests failed = {len(results_with_output) - len(passed_results)}")


def _log_benchmark_statistics(results: List[RunnerResult]) -> None:
    results_with_output = [
        result for result in results if result.get_status() != "exception"
    ]
    LOG.warning(f"Total number of benchmarks = {len(results)}")
    LOG.warning(f"Successfully finished benchmarks = {len(results_with_output)}")
    LOG.warning(
        f"Benchmarks finished with exceptions: {len(results) - len(results_with_output)}"  # noqa: line too long
    )


class ExitCode(enum.IntEnum):
    SUCCESS = 0
    FOUND_ERRORS = 1
    FAILURE = 2


def main(arguments: argparse.Namespace) -> int:
    specification_path: Optional[Path] = arguments.specification
    try:
        LOG.info("Reading input JSON...")
        if specification_path is None:
            file_content = sys.stdin.read()
        else:
            with open(specification_path, "r") as specification_file:
                file_content = specification_file.read()
        specification_jsons = json.loads(file_content)
        if not isinstance(specification_jsons, list):
            raise InvalidSpecificationException("Input JSON is not a list")

        LOG.info("Parsing JSON into test specification...")
        specifications = [
            Specification.from_json(input_json) for input_json in specification_jsons
        ]

        if arguments.benchmark:
            LOG.info(f"Start benchmarking {len(specifications)} specifications...")
            results = run_batch_benchmark(SubprocessEnvironment(), specifications)
            _log_benchmark_statistics(results)
            LOG.info("Done benchmarking.")
        else:
            LOG.info(f"Start testing {len(specifications)} specifications...")
            results = run_batch_test(SubprocessEnvironment(), specifications)
            _log_test_statistics(results)
            LOG.info("Done testing.")

        logger = arguments.logger
        if logger is None:
            report.to_console(results, arguments.dont_show_discrepancy)
        else:
            report.to_logger(logger, results, arguments.test_identifier)
    except FileNotFoundError:
        LOG.exception(f"Specification file at {specification_path} does not exist")
        return ExitCode.FAILURE
    except json.JSONDecodeError:
        LOG.exception(f"Cannot parse JSON at {specification_path}")
        return ExitCode.FAILURE
    except InvalidSpecificationException:
        LOG.exception("Invalid specification JSON")
        return ExitCode.FAILURE
    except Exception:
        LOG.exception("Exception occurs in the check")
        return ExitCode.FAILURE
    all_passed = all(result.get_status() == "pass" for result in results)
    return ExitCode.SUCCESS if all_passed else ExitCode.FOUND_ERRORS


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=(
            "Run incremental test for Pyre according to a list of test specifications"
        )
    )
    parser.add_argument(
        "specification",
        metavar="SPECIFICATION",
        type=Path,
        nargs="?",
        help="A JSON file containing a list of testing specifications",
    )
    parser.add_argument(
        "-b",
        "--benchmark",
        action="store_true",
        help=(
            "Only run incremental check and record the time. "
            "Do not run full check and compare results"
        ),
    )
    parser.add_argument(
        "--dont-show-discrepancy",
        action="store_true",
        help="Do not include error discrepancy in the result when the test fails",
    )
    parser.add_argument("-l", "--logger", type=str, help=argparse.SUPPRESS)
    parser.add_argument(
        "-i",
        "--test-identifier",
        type=str,
        help="An identifier to the run that makes it easy to filter results",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        help="Logging verbosity: 0 = only warnings, 1 = info, 2 = debug",
    )
    arguments: Namespace = parser.parse_args()
    _setup_logging(arguments.verbose)

    sys.exit(main(arguments))

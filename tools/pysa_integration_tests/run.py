# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import os
from pathlib import Path
from typing import List, Optional

# pyre-ignore[21]
from utils import run_pysa_integration_test

LOG: logging.Logger = logging.getLogger(__name__)


def main(
    run_directory: Path,
    filter_issues: bool,
    skip_model_verification: bool,
    run_from_source: bool,
    passthrough_args: List[str],
    save_results_to: Optional[Path],
) -> None:
    """
    Entry point function which checks if full_result.json is there, calls
    functions from integration_test_utils to run pysa, parse
    full_result.json, and compare the output.
    """
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s %(levelname)s %(message)s",
    )

    full_result_file_path = run_directory / "full_result.json"

    if not os.path.isfile(full_result_file_path):
        raise FileNotFoundError(
            f"{full_result_file_path} containing expected issues is not found"
        )

    LOG.info("Running in `%s`", run_directory)
    run_pysa_integration_test(
        run_directory,
        passthrough_args=passthrough_args,
        skip_model_verification=skip_model_verification,
        filter_issues=filter_issues,
        run_from_source=run_from_source,
        save_results_to=save_results_to,
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run integeration tests")

    parser.add_argument("--run-directory", type=Path)
    parser.add_argument("--filter-issues", action="store_true")
    parser.add_argument("--skip-model-verification", action="store_true")
    parser.add_argument("--run-from-source", action="store_true")
    parser.add_argument("--passthrough-args", nargs="+")
    parser.add_argument(
        "-s",
        "--save-results-to",
        type=Path,
        help=("Directory to write analysis results to. Default: output is not saved"),
    )

    parsed: argparse.Namespace = parser.parse_args()

    run_directory: Path = parsed.run_directory
    if run_directory is None:
        run_directory = Path(os.getcwd())
    filter_issues: bool = parsed.filter_issues
    skip_model_verification: bool = parsed.skip_model_verification
    run_from_source: bool = parsed.run_from_source
    passthrough_args: List[str] = parsed.passthrough_args
    if passthrough_args is None:
        passthrough_args = []
    save_results_to: Optional[Path] = parsed.save_results_to

    main(
        run_directory,
        filter_issues,
        skip_model_verification,
        run_from_source,
        passthrough_args,
        save_results_to,
    )

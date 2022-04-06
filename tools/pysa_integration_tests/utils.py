# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import json
import logging
import subprocess
import sys
from pathlib import Path
from typing import final, Optional, Sequence

LOG: logging.Logger = logging.getLogger(__name__)


@final
class PyreErrorException(Exception):
    """
    Custom Exception to raise when Pyre errors out
    """

    pass


def normalized_json_dump(
    results: str, salient_keys_only: bool, filter_issues: bool
) -> str:
    """
    Returns a normalised JSON string from results keeping only essential items.
    Removes all keys that are not salient to determining if results have changed
    when 'salient_keys_only' is true. Filters issues down to issues that have
    the code we intend to test for if 'filter_issues' is true.
    """
    normalized = json.loads(results)
    if "errors" in normalized:
        pretty_error = json.dumps(normalized, sort_keys=True, indent=2)
        raise PyreErrorException(
            f"Errors were found when processing results:\n{pretty_error}"
        )

    if filter_issues:
        # Filter down to only issues that have the code that we intended to
        # test for. This prevents the introduction of new rules or false
        # positives from breaking existing tests.
        normalized = [
            issue for issue in normalized if f"test_{issue['code']}_" in issue["define"]
        ]

    normalized = sorted(
        normalized,
        key=lambda issue: (
            issue["code"],
            issue["path"],
            issue["line"],
            issue["column"],
        ),
    )

    if salient_keys_only:
        salient_keys = {"code", "define", "description", "path"}
        stripped_issues = []
        for issue in normalized:
            stripped_issue = {
                key: value for key, value in issue.items() if key in salient_keys
            }
            if set(stripped_issue.keys()) != salient_keys:
                raise KeyError(
                    f"Expected issue to contain {salient_keys} keys, "
                    + f"but instead found: {issue}"
                )
            stripped_issues.append(stripped_issue)
        normalized = stripped_issues

    return json.dumps(normalized, sort_keys=True, indent=2) + "\n"


def compare_results(
    actual_results: str,
    expected_results: str,
    current_directory: Path,
    filter_issues: bool,
) -> None:
    normalized_pysa_results = normalized_json_dump(
        actual_results, salient_keys_only=True, filter_issues=filter_issues
    )
    normalized_expected_results = normalized_json_dump(
        expected_results, salient_keys_only=True, filter_issues=filter_issues
    )
    if normalized_pysa_results != normalized_expected_results:

        actual_full_results_path = current_directory / "full_result.actual"
        actual_full_results_path.write_text(
            normalized_json_dump(
                actual_results, salient_keys_only=False, filter_issues=filter_issues
            )
        )

        actual_invariant_results_path = (
            current_directory / "position_invariant_result.actual"
        )
        actual_invariant_results_path.write_text(normalized_pysa_results)

        expected_invariant_results_path = (
            current_directory / "position_invariant_result.json"
        )
        expected_invariant_results_path.write_text(normalized_expected_results)

        result = subprocess.run(
            [
                "diff",
                "-u",
                expected_invariant_results_path,
                actual_invariant_results_path,
            ],
            text=True,
            stdout=subprocess.PIPE,
        )
        friendly_exit(
            "Output differs from expected:",
            result.stdout,
            "output-differs-from-expected",
        )
    else:
        LOG.info("Run produced expected results")


def friendly_exit(error_message: str, logs: str, suggested_hash: str) -> None:
    """
    Error function to print error message using LOG and exit
    """
    LOG.error("----BEGIN PYSA INTEGRATION TEST ERROR----")
    LOG.error(error_message)
    LOG.error(logs)
    LOG.error("----END PYSA INTEGRATION TEST ERROR----")
    sys.exit(1)


def run_pysa_integration_test(
    current_directory: Path,
    passthrough_args: Sequence[str],
    skip_model_verification: bool,
    filter_issues: bool,
    save_results_to: Optional[Path],
    run_from_source: bool = False,
) -> None:
    """
    Runs pysa and compares the output to that in full_results.json. Creates
    raw_results.json file that contains the output. Creates
    position_invariant_result.json that contains position information to
    compare using diff with position_invariant_result.actual before exiting if
    there is a mismatch between the specified and detected issues.
    """
    LOG.info("Running `pyre analyze`")
    if run_from_source:
        command = [
            "python",
            "-m" "pyre-check.client.pyre",
        ]
    else:
        command = ["pyre"]
    command.extend(["--noninteractive", "analyze"])

    if save_results_to is not None:
        command.extend(["--save-results-to", str(save_results_to)])

    if skip_model_verification:
        command.append("--no-verify")

    command.extend(passthrough_args)
    LOG.debug(f"Using command: {command}")
    pysa_results: str
    try:
        pysa_results = subprocess.check_output(
            command, text=True, cwd=current_directory
        )
        if save_results_to is not None:
            pysa_results = (save_results_to / "errors.json").read_text()
    except subprocess.CalledProcessError as exception:
        friendly_exit(
            "Command failed with output:",
            exception.stdout,
            "found-x-model-verification-error",
        )

    (current_directory / "raw_results.json").write_text(pysa_results)

    expected_results = (current_directory / "full_result.json").read_text()

    compare_results(pysa_results, expected_results, current_directory, filter_issues)

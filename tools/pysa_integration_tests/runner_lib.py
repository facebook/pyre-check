# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Helper functions for integration test runners,
e.g running pysa and compare the results with the expectations.
"""

from __future__ import annotations

import ast
import enum
import json
import logging
import os.path
import re
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


@final
class TestConfigurationException(Exception):
    pass


class ExitCode(enum.IntEnum):
    # 1-29 reserved for pyre and pysa client, see client/commands/commands.py
    TEST_COMPARISON_DIFFERS = 30
    TEST_MODEL_VERIFICATION_ERROR = 31


def is_test_function(define: str, code: int) -> bool:
    return f"test_{code}_" in define


def is_test_class_method(define: str, code: int) -> bool:
    define_split = define.split(".")
    if len(define_split) < 2:
        return False
    return f"Test{code}" in define_split[-2]


def validate_test_functions_and_class_names(current_directory: Path) -> None:
    LOG.info(
        "Ensure all functions and classes in test_XXX files meet the expected format"
    )

    test_function_pattern = re.compile(r"test_\d{4}(_no)?_flag_\w+")
    test_class_pattern = re.compile(r"Test\d{4}\w+")
    helper_class_pattern = re.compile(r"Helper\w+")

    test_paths = [
        path
        for path in current_directory.iterdir()
        if re.match(r"test(_\w+)?\.py$", path.name)
    ]

    for test_path in test_paths:
        parsed_ast = ast.parse(test_path.read_text())

        functions = [
            node
            for node in parsed_ast.body
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        ]

        functions = []
        classes = []
        for node in parsed_ast.body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                functions.append(node)
            elif isinstance(node, ast.ClassDef):
                classes.append(node)

        for function in functions:
            function_name = function.name
            LOG.debug(f"Validating function: {function_name}")

            if not test_function_pattern.match(function_name):
                raise TestConfigurationException(
                    f"Expected test function {function_name} to have the "
                    + "format test_####_flag_XXXX or test_####_no_flag_XXXX, "
                    + "to indicate that issue #### is being tested"
                )

        for klass in classes:
            class_name = klass.name
            LOG.debug(f"Validating class: {class_name}")

            if not (
                test_class_pattern.match(class_name)
                or helper_class_pattern.match(class_name)
            ):
                raise TestConfigurationException(
                    f"Expected test class {class_name} to have the "
                    + "format Test####XXXX to indicate that issue #### "
                    + "is being tested, or HelperXXXX to indicate it is "
                    + "an unrelated helper class."
                )


def normalized_json_dump(
    results: str, ignore_positions: bool, filter_issues: bool
) -> str:
    """
    Returns a normalised JSON string from results keeping only essential items.
    Removes all keys that are not salient to determining if results have changed
    when `ignore_positions` is true. Filters issues down to issues that have
    the code we intend to test for if `filter_issues` is true.
    """
    normalized = json.loads(results)

    if "errors" in normalized:
        pretty_error = json.dumps(normalized, sort_keys=True, indent=2)
        raise PyreErrorException(
            f"Errors were found when processing analysis results:\n{pretty_error}"
        )

    if filter_issues:
        # Filter down to only issues that have the code that we intended to
        # test for. This prevents the introduction of new rules or false
        # positives from breaking existing tests.
        normalized = [
            issue
            for issue in normalized
            if is_test_function(issue["define"], issue["code"])
            or is_test_class_method(issue["define"], issue["code"])
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

    if ignore_positions:
        salient_keys = {"code", "define", "description", "path", "name"}
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


def run_pysa(
    *,
    save_results_to: Optional[Path] = None,
    save_errors_to: Optional[Path] = None,
    target: Optional[str] = None,
    number_of_workers: Optional[int] = None,
    skip_model_verification: bool = False,
    isolation_prefix: Optional[str] = None,
    repository_root: Optional[Path] = None,
    excludes: Optional[Sequence[str]] = None,
    run_from_source: bool = False,
    typeshed: Optional[Path] = None,
    compact_ocaml_heap: bool = False,
    check_invariants: bool = False,
    inline_decorators: bool = False,
    maximum_trace_length: Optional[int] = None,
    maximum_tito_depth: Optional[int] = None,
    passthrough_args: Optional[Sequence[str]] = None,
    working_directory: Optional[Path] = None,
    silent: bool = False,
    error_help: Optional[str] = None,
) -> str:
    """Run pysa for the given test and produce a list of errors in JSON."""
    if run_from_source:
        command = [
            "python",
            "-m" "pyre-check.client.pyre",
        ]
    else:
        command = ["pyre"]

    command.append("--noninteractive")

    if isolation_prefix is not None:
        command.extend(["--isolation-prefix", isolation_prefix])

    if number_of_workers is not None:
        command.append(f"--number-of-workers={number_of_workers}")

    if typeshed is not None:
        command.extend(["--typeshed", typeshed.absolute().as_posix()])

    if target is not None:
        command.append(f"--target={target}")

    if excludes is not None:
        for exclude in excludes:
            command.extend(["--exclude", exclude])

    command.append("analyze")

    if skip_model_verification:
        command.append("--no-verify")

    if repository_root is not None:
        command.extend(["--repository-root", str(repository_root)])

    if save_results_to is not None:
        command.extend(["--save-results-to", str(save_results_to)])

    if compact_ocaml_heap:
        command.append("--compact-ocaml-heap")

    if check_invariants:
        command.append("--check-invariants")

    if inline_decorators:
        command.append("--inline-decorators")

    if maximum_trace_length is not None:
        command.append(f"--maximum-trace-length={maximum_trace_length}")

    if maximum_tito_depth is not None:
        command.append(f"--maximum-tito-depth={maximum_tito_depth }")

    if passthrough_args is not None:
        command.extend(passthrough_args)

    LOG.info(f"Running `{' '.join(command)}`")
    try:
        process = subprocess.run(
            command,
            check=True,
            text=True,
            stdout=subprocess.PIPE,
            stderr=(subprocess.DEVNULL if silent else None),
            cwd=working_directory,
        )
    except subprocess.CalledProcessError as exception:
        LOG.error(f"`pyre analyze` failed with return code {exception.returncode}")
        sys.stdout.write(exception.stdout)
        if error_help is not None:
            sys.stdout.write("\n")
            sys.stdout.write(error_help)
        sys.exit(exception.returncode)

    if save_results_to is not None:
        errors = (save_results_to / "errors.json").read_text()
    else:
        errors = process.stdout

    if save_errors_to is not None:
        save_errors_to.write_text(errors)

    return errors


def compare_to_expected_json(
    *,
    actual_results: str,
    expected_results_path: Path,
    test_result_directory: Path,
    filter_issues: bool,
    ignore_positions: bool,
    error_help: Optional[str] = None,
) -> None:
    """
    Compare the errors from `run_pysa` to a set of expected
    errors from a JSON file.
    """
    if not os.path.isfile(expected_results_path):
        raise FileNotFoundError(
            f"Could NOT find expected result file `{expected_results_path}`"
        )
    expected_results = expected_results_path.read_text()

    normalized_pysa_results = normalized_json_dump(
        actual_results, ignore_positions=ignore_positions, filter_issues=filter_issues
    )
    normalized_expected_results = normalized_json_dump(
        expected_results, ignore_positions=ignore_positions, filter_issues=filter_issues
    )
    if normalized_pysa_results == normalized_expected_results:
        LOG.info("Run produced expected results")
        return

    (test_result_directory / "full_result.json").write_text(actual_results)
    (test_result_directory / "result.actual").write_text(
        normalized_json_dump(
            actual_results, ignore_positions=False, filter_issues=filter_issues
        )
    )

    if ignore_positions:
        actual_invariant_results_path = (
            test_result_directory / "position_invariant_result.actual"
        )
        actual_invariant_results_path.write_text(normalized_pysa_results)

        expected_invariant_results_path = (
            test_result_directory / "position_invariant_result.json"
        )
        expected_invariant_results_path.write_text(normalized_expected_results)
    else:
        actual_invariant_results_path = test_result_directory / "result.actual"
        expected_invariant_results_path = expected_results_path

    if ignore_positions:
        sys.stdout.write("Output differs from expected:\n")
    else:
        sys.stdout.write("Output differs from expected (after stripping locations):\n")
    sys.stdout.flush()
    subprocess.run(
        [
            "diff",
            "-u",
            expected_invariant_results_path,
            actual_invariant_results_path,
        ]
    )
    if error_help is not None:
        sys.stdout.write("\n")
        sys.stdout.write(error_help)
    sys.exit(ExitCode.TEST_COMPARISON_DIFFERS.value)

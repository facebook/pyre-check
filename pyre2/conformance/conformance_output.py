# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import argparse
import difflib
import json
import logging
import os
import re
import subprocess
import sys
import tempfile
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List

logger: logging.Logger = logging.getLogger(__name__)

# We want to tag a bunch of the output files as generated, but don't want this
# file to be tagged as generated.
AT_GENERATED: str = "@" + "generated"

"""
This script collects the Pyre errors for the conformance test suite and updates the ".exp" and ".result" files.
When run with the --check flag, it will check that the ".exp" files are up-to-date.
When run with the --compare flag, it will compare Pyre's output with the expected errors in the conformance test sources.
"""

# this contains the errors that Pyre emits for each test case
EXPECTED_OUTPUT = "conformance.exp"
# this contains the differences between Pyre's errors and the errors that the test case expects
COMPARE_RESULT = "conformance.result"
# this contains a summary of the passing and failing test cases, and the mismatch count for each test case
SUMMARY_FILE = "results.json"


def is_excluded(test_case: str) -> bool:
    return test_case.startswith("_")


# Update a file, but don't do a partial update, to avoid the file
# temporarily being marked dirty by version control systems.
def update_file(file_path: str, content: str) -> None:
    if Path(file_path).exists():
        # lint-ignore: NoUnsafeFilesystemRule
        with open(file_path, "r") as f:
            old_content = f.read()
    else:
        old_content = None
    if content != old_content:
        # lint-ignore: NoUnsafeFilesystemRule
        with open(file_path, "w") as f:
            f.write(content)


@dataclass(frozen=True)
class ExpectedErrors:
    # line -> number of required errors on line
    required_errors: Dict[int, int]
    # line -> number of optional errors on line
    optional_errors: Dict[int, int]
    # error tag -> lines where that error may appear
    error_groups: Dict[str, List[int]]


def get_expected_errors(test_case: str) -> ExpectedErrors:
    """
    Return the line numbers where type checkers are expected to produce an error.

    The return value is a tuple of two dictionaries:
    - The format of the first is {line number: (number of required errors, number of optional errors)}.
    - The format of the second is {error tag: [lines where the error may appear]}.

    For example, the following test case:

        x: int = "x"  # E
        y: int = "y"  # E?
        @final  # E[final]
        def f(): pass  # E[final]

    will return:

        (
            {1: (1, 0), 2: (0, 1)},
            {"final": [3, 4]}
        )

    https://github.com/python/typing/blob/main/conformance/src/main.py
    """
    # lint-ignore: NoUnsafeFilesystemRule
    with open(test_case, "r") as f:
        lines = f.readlines()
    required_errors: Dict[int, int] = {}
    optional_errors: Dict[int, int] = {}
    error_groups: Dict[str, List[int]] = {}
    for i, line in enumerate(lines, start=1):
        line_without_comment, *_ = line.split("#")
        # Ignore lines with no non-comment content. This allows commenting out test cases.
        if not line_without_comment.strip():
            continue
        required = 0
        optional = 0
        for match in re.finditer(r"# E\??(?=:|$| )", line):
            if match.group() == "# E":
                required += 1
            else:
                optional += 1
        if required or optional:
            required_errors[i] = required
            optional_errors[i] = optional
        for match in re.finditer(r"# E\[([^\]]+)\]", line):
            error_groups.setdefault(match.group(1), []).append(i)
    for group, linenos in error_groups.items():
        if len(linenos) == 1:
            raise ValueError(
                f"Error group {group} only appears on a single line in {test_case}"
            )
    return ExpectedErrors(
        required_errors=required_errors,
        optional_errors=optional_errors,
        error_groups=error_groups,
    )


def diff_expected_errors(directory: str, test_case: str) -> List[str]:
    """
    Return a list of errors that were expected but not produced by the type checker.

    https://github.com/python/typing/blob/main/conformance/src/main.py
    """
    expected_errors = get_expected_errors(test_case)
    output_path = os.path.join(directory, EXPECTED_OUTPUT)
    errors = defaultdict(lambda: [])
    if os.path.exists(output_path):
        # lint-ignore: NoUnsafeFilesystemRule
        with open(output_path, "r") as f:
            f.readline()
            for error in json.load(f)[test_case.split("/")[-1]]:
                if error["code"] != -1:
                    errors[error["line"]].append(error["description"])
    differences: list[str] = []
    for expected_lineno, required_errors in expected_errors.required_errors.items():
        if expected_lineno not in errors and required_errors > 0:
            differences.append(
                f"Line {expected_lineno}: Expected {required_errors} errors"
            )
    linenos_used_by_groups: set[int] = set()
    for group, linenos in expected_errors.error_groups.items():
        if not any(lineno in errors for lineno in linenos):
            differences.append(
                f"Lines {', '.join(map(str, linenos))}: Expected error (tag {group!r})"
            )
        else:
            linenos_used_by_groups.update(linenos)
    for actual_lineno, actual_errors in errors.items():
        if (
            actual_lineno not in expected_errors.required_errors
            and actual_lineno not in expected_errors.optional_errors
            and actual_lineno not in linenos_used_by_groups
        ):
            differences.append(
                f"Line {actual_lineno}: Unexpected errors {actual_errors}"
            )
    return differences


def compare_conformance_output(
    directory: str, conformance_output: Dict[str, List[dict[str, str]]]
) -> Dict[str, List[str]]:
    """
    Compare conformance output with expected results in conformance test sources.
    Returns a mapping of files to error messages if there are discrepancies.
    """
    messages = defaultdict(lambda: [])
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith(".py") and not is_excluded(file):
                file_path = os.path.join(root, file)
                messages_for_file = diff_expected_errors(directory, file_path)
                messages[file_path].append(messages_for_file)
    return messages


def get_pyre2_command(test: bool) -> list[str]:
    command = ["buck2"]
    if test:
        command += ["--isolation-dir", "pyre2"]
    return command + [
        "run",
        "--reuse-current-config",
        "fbcode//tools/pyre/pyre2:pyre2",
        "--",
        "expect-test",
    ]


def get_conformance_output(
    directory: str, test: bool
) -> Dict[str, List[Dict[str, Any]]]:
    """
    Run minpyre on conformance test suite, parse and group the output by file
    """
    files_to_check = []
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith(".py") and not is_excluded(file):
                files_to_check.append(os.path.join(root, file))
    outputs = defaultdict(lambda: [])
    with tempfile.NamedTemporaryFile() as tmp_file:
        cmd = (
            get_pyre2_command(test)
            + [
                "--output",
                tmp_file.name,
                "--output-format=json",
            ]
            + files_to_check
        )
        # lint-ignore: NoUnsafeExecRule
        result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stderr = result.stderr.decode()
        try:
            # lint-ignore: NoUnsafeFilesystemRule
            with open(tmp_file.name, "r") as tmp_file:
                errors = json.load(tmp_file)
                for error in errors["errors"]:
                    path = error["path"]
                    del error["path"]
                    outputs[os.path.normpath(path)].append(error)
        except Exception:
            logger.error("Failed to get conformance output\n{}\n".format(stderr))
    return outputs


def get_conformance_output_separate(
    directory: str, test: bool
) -> Dict[str, List[Dict[str, Any]]]:
    """
    Run minpyre on conformance test suite, parse and group the output by file
    This function runs Pyre2 separately for each file, which is slower but more robust to failures
    """
    files_to_check = []
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith(".py") and not is_excluded(file):
                files_to_check.append(os.path.join(root, file))
    outputs = defaultdict(lambda: [])
    for file in files_to_check:
        with tempfile.NamedTemporaryFile() as tmp_file:
            cmd = (
                get_pyre2_command(test)
                + [
                    "--output",
                    tmp_file.name,
                ]
                + [file]
            )
            # lint-ignore: NoUnsafeExecRule
            result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            stderr = result.stderr.decode()
            try:
                # lint-ignore: NoUnsafeFilesystemRule
                with open(tmp_file.name, "r") as tmp_file:
                    errors = json.load(tmp_file)
                    for error in errors["errors"]:
                        path = error["path"]
                        del error["path"]
                        outputs[os.path.normpath(path)].append(error)
            except Exception:
                logger.error(
                    "Failed to get conformance output for {}\n{}\n".format(file, stderr)
                )
    return outputs


def collect_test_cases(directory: str) -> List[str]:
    test_cases = []
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith(".py") and not is_excluded(file):
                python_file_path = os.path.join(root, file)
                if os.path.exists(python_file_path):
                    test_cases.append(os.path.normpath(python_file_path))
    return sorted(test_cases)


def main() -> None:
    parser = argparse.ArgumentParser(description="Process some files in a directory")
    parser.add_argument(
        "directory", help="path to directory containing files to process"
    )
    parser.add_argument(
        "--mode", "-m", choices=["update", "check", "compare", "test"], default="update"
    )
    parser.add_argument(
        "--separate", action="store_true", help="run Pyre2 separately for each case"
    )
    args = parser.parse_args()
    if args.separate:
        conformance_output = get_conformance_output_separate(
            directory=args.directory,
            test=args.mode == "test",
        )
    else:
        conformance_output = get_conformance_output(
            directory=args.directory,
            test=args.mode == "test",
        )
    if len(conformance_output) == 0:
        logger.error(f"Failed to get conformance output for directory {args.directory}")
        sys.exit(1)
    if not os.path.exists(args.directory):
        logger.error(f"Directory {args.directory} does not exist")
        sys.exit(1)
    if args.mode == "check" or args.mode == "test":
        messages = []
        test_cases = collect_test_cases(args.directory)
        expected_output_path = os.path.join(args.directory, EXPECTED_OUTPUT)
        current_output = (
            AT_GENERATED
            + "\n"
            + json.dumps(
                {test.split("/")[-1]: conformance_output[test] for test in test_cases},
                indent=2,
                sort_keys=True,
            ).strip()
        )
        old_output = Path(expected_output_path).read_text().strip()
        if current_output != old_output:
            differ = difflib.Differ()
            difference = differ.compare(
                current_output.split("\n"), old_output.split("\n")
            )
            prefixes = {"+", "-", "?"}
            difference = [d for d in difference if d[0] in prefixes]
            messages.append("\n".join(difference) + "\n")
        if len(messages) > 0:
            logger.error(
                "Conformance output is not up to date. Please cd to fbcode/tools/pyre/pyre2/conformance/\n"
                + "and re-generate the output with `buck2 run :conformance_output_script -- ./third_party`.\n"
                + "\n".join(messages)
            )
            sys.exit(1)
    elif args.mode == "update":
        n_pass = 0
        n_fail = 0
        n_differences = 0
        passing = []
        failing = {}
        test_cases = collect_test_cases(args.directory)
        # emit .exp files
        expected_output_path = os.path.join(args.directory, EXPECTED_OUTPUT)
        update_file(
            expected_output_path,
            AT_GENERATED
            + "\n"
            + json.dumps(
                {test.split("/")[-1]: conformance_output[test] for test in test_cases},
                indent=2,
                sort_keys=True,
            ),
        )
        # emit .result files and results.json
        expected_output_path = os.path.join(args.directory, COMPARE_RESULT)
        diff_output = {}
        for path in test_cases:
            diff = diff_expected_errors(args.directory, path)
            if len(diff) == 0:
                passing.append(path.split("/")[-1])
                n_pass += 1
            else:
                n_differences += len(diff)
                failing[path.split("/")[-1]] = len(diff)
                n_fail += 1
            diff_output[path.split("/")[-1]] = diff
        update_file(
            expected_output_path,
            AT_GENERATED + "\n" + json.dumps(diff_output, indent=2, sort_keys=True),
        )
        update_file(
            os.path.join(args.directory, SUMMARY_FILE),
            json.dumps(
                {
                    "total": n_pass + n_fail,
                    "pass": n_pass,
                    "fail": n_fail,
                    "pass_rate": round(n_pass / (n_pass + n_fail), 2),
                    "differences": n_differences,
                    "passing": sorted(passing),
                    "failing": failing,
                    "comment": AT_GENERATED,
                },
                indent=2,
            ),
        )
    elif args.mode == "compare":
        messages = compare_conformance_output(args.directory, conformance_output)
        print(json.dumps(messages, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()

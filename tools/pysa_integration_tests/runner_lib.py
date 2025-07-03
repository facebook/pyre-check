# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Helper functions for integration test runners,
e.g running pysa and compare the results with the expectations.
"""

from __future__ import annotations as _annotations

import ast
import collections
import enum
import json
import logging
import os.path
import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import (
    DefaultDict,
    Dict,
    final,
    IO,
    List,
    Optional,
    Sequence,
    TypedDict,
    Union,
)

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
    helper_function_pattern = re.compile(r"helper\w+")
    test_class_pattern = re.compile(r"Test\d{4}\w+")
    helper_class_pattern = re.compile(r"Helper\w+")

    test_paths = [
        path
        for path in current_directory.glob("**/*.py")
        if re.match(r"test(_\w+)?\.py$", path.name)
    ]

    for test_path in test_paths:
        parsed_ast = ast.parse(test_path.read_text())

        functions = []
        classes = []
        for node in parsed_ast.body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                functions.append(node)
            elif isinstance(node, ast.ClassDef):
                classes.append(node)

        # Note: this only iterates on top level functions, excluding methods
        # and nested functions.
        for function in functions:
            function_name = function.name
            LOG.debug(f"Validating function: {function_name}")

            if helper_function_pattern.match(function_name):
                continue

            if test_function_pattern.match(function_name):
                # Sanity check that there is at least one test annotation.
                if not any(
                    isinstance(decorator, ast.Call)
                    and isinstance(decorator.func, ast.Name)
                    and decorator.func.id in ("ExpectIssue", "ExpectNoIssue")
                    for decorator in function.decorator_list
                ):
                    raise TestConfigurationException(
                        f"Test function {function_name} does NOT have any test annotation (`ExpectIssue`, `ExpectNoIssue`)"
                    )

                continue

            raise TestConfigurationException(
                f"Expected test function {function_name} to have the "
                + "format test_####_flag_XXXX or test_####_no_flag_XXXX, "
                + "to indicate that issue #### is being tested, or "
                + "helperXXX to indicate it is an unrelated helper "
                + "function."
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
    maximum_trace_length: Optional[int] = None,
    maximum_tito_depth: Optional[int] = None,
    passthrough_args: Optional[Sequence[str]] = None,
    working_directory: Optional[Path] = None,
    silent: bool = False,
    shard_taint_output: bool = False,
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

    if maximum_trace_length is not None:
        command.append(f"--maximum-trace-length={maximum_trace_length}")

    if maximum_tito_depth is not None:
        command.append(f"--maximum-tito-depth={maximum_tito_depth }")

    if shard_taint_output:
        command.append("--output-format=sharded-json")

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
    write_actual_results_on_failure: bool,
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

    actual_results_path = test_result_directory / "result.actual"

    (test_result_directory / "full_result.json").write_text(actual_results)
    actual_results_path.write_text(
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

    if write_actual_results_on_failure:
        sys.stdout.write("Contents of result.actual:\n")
        sys.stdout.write("---\n")
        sys.stdout.write(actual_results_path.read_text())
        sys.stdout.write("---\n")
    sys.exit(ExitCode.TEST_COMPARISON_DIFFERS.value)


@dataclass(frozen=True)
class TestAnnotation:
    expected: bool  # True = ExpectIssue, False = ExpectNoIssue
    code: int
    line: Optional[int] = None
    task: Optional[str] = None
    currently_found: Optional[bool] = None

    def asdict(self) -> Dict[str, Union[bool, int, str]]:
        result: Dict[str, Union[bool, int, str]] = {
            "expected": self.expected,
            "code": self.code,
        }
        line = self.line
        if line is not None:
            result["line"] = line
        task = self.task
        if task is not None:
            result["task"] = task
        currently_found = self.currently_found
        if currently_found is not None:
            result["currently_found"] = currently_found
        return result


@dataclass(frozen=True)
class FunctionTestAnnotations:
    definition_line: int
    annotations: List[TestAnnotation]


class DirectoryTestAnnotations:
    def __init__(self) -> None:
        self.function_annotations: Dict[str, FunctionTestAnnotations] = {}

    def set(self, function: str, annotations: FunctionTestAnnotations) -> None:
        self.function_annotations[function] = annotations

    def add(self, other: "DirectoryTestAnnotations") -> None:
        for name, annotations in other.function_annotations.items():
            if name in self.function_annotations:
                raise AssertionError(
                    f"Could NOT merge annotations with conflicting definitions: {name}"
                )
            self.function_annotations[name] = annotations

    def number_annotations(self) -> int:
        return sum(
            len(function_annotations.annotations)
            for function_annotations in self.function_annotations.values()
        )

    def dump(self, output: IO[str]) -> None:
        output.write(
            json.dumps(
                {
                    name: [
                        annotation.asdict()
                        for annotation in function_annotations.annotations
                    ]
                    for name, function_annotations in self.function_annotations.items()
                }
            )
        )
        output.write("\n")


def parse_test_annotation(
    decorator: ast.Call, decorated_function: str
) -> Optional[TestAnnotation]:
    if not isinstance(decorator.func, ast.Name):
        return None

    expected: bool = True
    if decorator.func.id == "ExpectIssue":
        expected = True
    elif decorator.func.id == "ExpectNoIssue":  # pyre-ignore
        expected = False
    else:
        # Not a test annotation.
        return None

    code: Optional[int] = None
    line: Optional[int] = None
    task: Optional[str] = None
    currently_found: Optional[bool] = None

    if len(decorator.args) > 0:
        raise TestConfigurationException(
            f"Invalid annotation `{decorator.func.id}` on `{decorated_function}`: "
            + "Unsupported positional argument"
        )

    for keyword_argument in decorator.keywords:
        if keyword_argument.arg is None:
            raise TestConfigurationException(
                f"Invalid annotation `{decorator.func.id}` on `{decorated_function}`: "
                + "Unsupported **kwargs"
            )
        if keyword_argument.arg == "code":
            if not isinstance(keyword_argument.value, ast.Constant) or not isinstance(
                keyword_argument.value.value, int
            ):
                raise TestConfigurationException(
                    f"Invalid annotation `{decorator.func.id}` on `{decorated_function}`: "
                    + f"Invalid type for parameter {keyword_argument.arg}"
                )
            code = keyword_argument.value.value
        elif keyword_argument.arg == "line":
            if not isinstance(keyword_argument.value, ast.Constant) or not isinstance(
                keyword_argument.value.value, int
            ):
                raise TestConfigurationException(
                    f"Invalid annotation `{decorator.func.id}` on `{decorated_function}`: "
                    + f"Invalid type for parameter {keyword_argument.arg}"
                )
            line = keyword_argument.value.value
        elif keyword_argument.arg == "task":
            if not isinstance(keyword_argument.value, ast.Constant) or not isinstance(
                keyword_argument.value.value, str
            ):
                raise TestConfigurationException(
                    f"Invalid annotation `{decorator.func.id}` on `{decorated_function}`: "
                    + f"Invalid type for parameter {keyword_argument.arg}"
                )
            task = keyword_argument.value.value
        elif keyword_argument.arg == "currently_found":
            if not isinstance(keyword_argument.value, ast.Constant) or not isinstance(
                keyword_argument.value.value, bool
            ):
                raise TestConfigurationException(
                    f"Invalid annotation `{decorator.func.id}` on `{decorated_function}`: "
                    + f"Invalid type for parameter {keyword_argument.arg}"
                )
            currently_found = keyword_argument.value.value
        else:
            raise TestConfigurationException(
                f"Invalid annotation `{decorator.func.id}` on `{decorated_function}`: "
                + f"Unexpected parameter {keyword_argument.arg}"
            )

    if code is None:
        raise TestConfigurationException(
            f"Invalid annotation `{decorator.func.id}` on `{decorated_function}`: "
            + "Missing required parameter 'code'"
        )

    return TestAnnotation(
        expected=expected,
        code=code,
        line=line,
        task=task,
        currently_found=currently_found,
    )


class FunctionDefinitions:
    def __init__(self) -> None:
        self.functions: Dict[str, Union[ast.FunctionDef, ast.AsyncFunctionDef]] = {}

    def add_function(
        self, name: str, function: Union[ast.FunctionDef, ast.AsyncFunctionDef]
    ) -> None:
        self.functions[name] = function

    @staticmethod
    def from_ast(parsed_ast: ast.AST) -> "FunctionDefinitions":
        functions = FunctionDefinitions()
        functions.add_from_statements(
            parsed_ast.body,  # pyre-ignore: _ast.AST has no attribute body
            prefix="",
        )
        return functions

    def add_from_statements(self, statements: List[ast.stmt], prefix: str) -> None:
        for statement in statements:
            if isinstance(statement, (ast.FunctionDef, ast.AsyncFunctionDef)):
                self.add_function(f"{prefix}{statement.name}", statement)
                self.add_from_statements(
                    statement.body, prefix=f"{prefix}{statement.name}."
                )
            elif isinstance(statement, ast.ClassDef):
                self.add_from_statements(
                    statement.body, prefix=f"{prefix}{statement.name}."
                )


def parse_test_annotations_from_source(
    source: str,
) -> Dict[str, FunctionTestAnnotations]:
    parsed_ast = ast.parse(source)

    functions = FunctionDefinitions.from_ast(parsed_ast)

    annotated_functions: Dict[str, FunctionTestAnnotations] = {}
    for qualified_name, function in functions.functions.items():
        annotations: List[TestAnnotation] = []
        for decorator_expression in function.decorator_list:
            if not isinstance(decorator_expression, ast.Call):
                continue

            annotation = parse_test_annotation(decorator_expression, function.name)
            if annotation is not None:
                annotations.append(annotation)

        if len(annotations) > 0:
            annotated_functions[qualified_name] = FunctionTestAnnotations(
                definition_line=function.lineno, annotations=annotations
            )

    # Sanity check that we parsed all test annotations.
    number_expect_issue_substrings = source.count("@ExpectIssue(")
    number_expect_no_issue_substrings = source.count("@ExpectNoIssue(")
    number_parsed_annotations = sum(
        len(function_annotation.annotations)
        for function_annotation in annotated_functions.values()
    )
    if (
        number_expect_issue_substrings + number_expect_no_issue_substrings
        != number_parsed_annotations
    ):
        raise TestConfigurationException(
            "Unexpected mismatch between '@ExpectIssue' and parsed annotations:\n"
            + f"Found {number_expect_issue_substrings} @ExpectIssue\n"
            + f"Found {number_expect_no_issue_substrings} @ExpectNoIssue\n"
            + f"Parsed {number_parsed_annotations} test annotations"
        )

    return annotated_functions


def parse_test_annotations_from_directory(
    directory: Path, repository_root: Path
) -> DirectoryTestAnnotations:
    LOG.info(f"Parsing test annotations in {directory}")

    result = DirectoryTestAnnotations()
    for path in directory.glob("**/*.py"):
        if path.name == "runner_lib.py":
            continue

        base_module = ".".join(path.relative_to(repository_root).parts)
        base_module = base_module[:-3]  # Remove .py suffix

        for function_name, annotations in parse_test_annotations_from_source(
            path.read_text()
        ).items():
            result.set(f"{base_module}.{function_name}", annotations)

    LOG.info(f"Found {result.number_annotations()} test annotations")

    if result.number_annotations() == 0:
        raise TestConfigurationException(
            f"Could NOT find test annotations in {directory}"
        )

    return result


class Issue(TypedDict):
    define: str
    code: int
    path: str
    line: int
    column: int
    stop_line: int
    stop_column: int
    description: str
    name: str


def compare_issues_to_test_annotations(
    function: str,
    definition_line: int,
    code: int,
    issues: List[Issue],
    annotations: List[TestAnnotation],
) -> List[str]:
    remaining_issues = sorted(
        issues, key=lambda issue: (issue["line"], issue["column"])
    )
    annotations.sort(key=lambda annotation: annotation.line or 99999)

    test_failures: List[str] = []
    number_expected_issues = 0
    for annotation in annotations:
        currently_found = annotation.currently_found
        expected = (
            currently_found if currently_found is not None else annotation.expected
        )
        expected_reason = (
            f" (because currently_found={currently_found})"
            if currently_found is not None
            else ""
        )

        line = annotation.line
        matching_issues_index: List[int] = [
            index
            for index, issue in enumerate(remaining_issues)
            if line is None or issue["line"] - definition_line == line
        ]
        line_expectation_prefix = f" at line {line}" if line is not None else ""
        line_expectation_suffix = " at that line" if line is not None else ""

        if expected:
            number_expected_issues += 1
            if len(matching_issues_index) > 0:  # pass
                remaining_issues.pop(matching_issues_index[0])
            else:  # failure
                test_failures.append(
                    f"Expected an issue with code {code}{line_expectation_prefix}{expected_reason}, but no issue was found{line_expectation_suffix}."
                )
        else:
            if len(matching_issues_index) > 0:  # failure
                test_failures.append(
                    f"Expected NO issue with code {code}{line_expectation_prefix}{expected_reason}, but an issue was found{line_expectation_suffix}."
                )
            else:
                pass  # pass

    if len(issues) != number_expected_issues and len(test_failures) == 0:
        test_failures.append(
            f"Expected {number_expected_issues} issues with code {code}, but found {len(issues)} issues."
        )

    if len(test_failures) > 0:
        if len(issues) > 0:
            issues_for_json = [
                {
                    "description": issue["description"],
                    "name": issue["name"],
                    "start": "%d:%d" % (issue["line"], issue["column"]),
                    "end": "%d:%d" % (issue["stop_line"], issue["stop_column"]),
                    "relative_line": issue["line"] - definition_line,
                }
                for issue in issues
            ]
            test_failures.append(
                f"Issues with code {code} for callable `{function}`:\n{json.dumps(issues_for_json, indent=2)}"
            )
        else:
            test_failures.append(f"No issue found with code {code} for {function}")

    return test_failures


def compare_to_test_annotations(
    *,
    actual_errors: List[Issue],
    annotations: DirectoryTestAnnotations,
    error_help: Optional[str] = None,
) -> None:
    """
    Compare the errors from `run_pysa` to the test expectations
    from test annotations.
    """
    function_to_code_to_issues: DefaultDict[str, DefaultDict[int, List[Issue]]] = (
        collections.defaultdict(lambda: collections.defaultdict(lambda: []))
    )
    for error in actual_errors:
        function_to_code_to_issues[error["define"]][error["code"]].append(error)

    test_failures: List[str] = []
    for function, function_annotations in annotations.function_annotations.items():
        annotations_per_code: DefaultDict[int, List[TestAnnotation]] = (
            collections.defaultdict(lambda: [])
        )
        for annotation in function_annotations.annotations:
            annotations_per_code[annotation.code].append(annotation)

        function_test_failures: List[str] = []
        for code, annotations_for_code in annotations_per_code.items():
            issues = function_to_code_to_issues[function][code]
            function_test_failures += compare_issues_to_test_annotations(
                function,
                function_annotations.definition_line,
                code,
                issues,
                annotations_for_code,
            )

        if len(function_test_failures) > 0:
            test_failures.append(
                f"Error{'s' if len(function_test_failures) > 1 else ''} for `{function}`:"
            )
            test_failures += function_test_failures

    if len(test_failures) == 0:
        LOG.info("Run produced expected results")
        return

    sys.stdout.write("Output differs from expected:\n")
    for test_failure in test_failures:
        sys.stdout.write(test_failure + "\n")
    if error_help is not None:
        sys.stdout.write("\n")
        sys.stdout.write(error_help)
    sys.exit(ExitCode.TEST_COMPARISON_DIFFERS.value)

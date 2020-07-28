# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import itertools
import json
import logging
import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Tuple

from . import UserError, ast


LOG: logging.Logger = logging.getLogger(__name__)
MAX_LINES_PER_FIXME: int = 4


class PartialErrorSuppression(Exception):
    def __init__(self, message: str, unsuppressed_paths: List[str]) -> None:
        super().__init__(message)
        self.unsuppressed_paths: List[str] = unsuppressed_paths


def error_path(error: Dict[str, Any]) -> str:
    return error["path"]


class Errors:
    @classmethod
    def empty(cls) -> "Errors":
        return cls([])

    @staticmethod
    def from_json(
        json_string: str,
        only_fix_error_code: Optional[int] = None,
        from_stdin: bool = False,
    ) -> "Errors":
        try:
            errors = json.loads(json_string)
            return Errors(_filter_errors(errors, only_fix_error_code))
        except json.decoder.JSONDecodeError:
            if from_stdin:
                raise UserError(
                    "Received invalid JSON as input. "
                    "If piping from `pyre check` be sure to use `--output=json`."
                )
            else:
                raise UserError(
                    f"Encountered invalid output when checking for pyre errors: `{json_string}`."
                )

    @staticmethod
    def from_stdin(only_fix_error_code: Optional[int] = None) -> "Errors":
        input = sys.stdin.read()
        return Errors.from_json(input, only_fix_error_code, from_stdin=True)

    def __init__(self, errors: List[Dict[str, Any]]) -> None:
        self.errors: List[Dict[str, Any]] = errors
        self.error_iterator: Iterator[
            Tuple[str, Iterator[Dict[str, Any]]]
            # pyre-fixme[6]: Expected `(_T) -> _SupportsLessThan` for 2nd param but got
            #  `(error: Dict[str, typing.Any]) -> str`.
        ] = itertools.groupby(sorted(errors, key=error_path), error_path)
        self.length: int = len(errors)

    def __iter__(self) -> Iterator[Tuple[str, Iterator[Dict[str, Any]]]]:
        return self.error_iterator.__iter__()

    def __next__(self) -> Tuple[str, Iterator[Dict[str, Any]]]:
        return self.error_iterator.__next__()

    def __len__(self) -> int:
        return self.length

    def __eq__(self, other: "Errors") -> bool:
        return self.errors == other.errors

    def suppress(
        self,
        comment: Optional[str] = None,
        max_line_length: Optional[int] = None,
        truncate: bool = False,
        unsafe: bool = False,
    ) -> None:
        unsuppressed_paths = []

        for path_to_suppress, errors in self:
            LOG.info("Processing `%s`", path_to_suppress)
            try:
                path = Path(path_to_suppress)
                input = path.read_text()
                output = _suppress_errors(
                    input,
                    _build_error_map(errors),
                    comment,
                    max_line_length
                    if max_line_length and max_line_length > 0
                    else None,
                    truncate,
                    unsafe,
                )
                path.write_text(output)
            except SkippingGeneratedFileException:
                LOG.warning(f"Skipping generated file at {path_to_suppress}")
            except ast.UnstableAST:
                unsuppressed_paths.append(path_to_suppress)

        if unsuppressed_paths:
            paths_string = ", ".join(unsuppressed_paths)
            raise PartialErrorSuppression(
                f"Could not fully suppress errors in: {paths_string}",
                unsuppressed_paths,
            )


def _filter_errors(
    errors: List[Dict[str, Any]], only_fix_error_code: Optional[int] = None
) -> List[Dict[str, Any]]:
    if only_fix_error_code is not None:
        errors = [error for error in errors if error["code"] == only_fix_error_code]
    return errors


def errors_from_targets(
    project_directory: Path,
    path: str,
    targets: List[str],
    check_alternate_names: bool = True,
) -> Errors:
    buck_test_command = (
        ["buck", "test", "--show-full-json-output"] + targets + ["--", "--run-disabled"]
    )
    buck_test = subprocess.run(
        buck_test_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    errors = Errors.empty()
    if buck_test.returncode == 0:
        # Successful run with no type errors
        LOG.info("No errors in %s...", path)
    elif buck_test.returncode == 32:
        buck_test_output = buck_test.stdout.decode().split("\n")
        pyre_error_pattern = re.compile(r"\W*(.*\.pyi?):(\d*):(\d*) (.* \[(\d*)\]: .*)")
        errors = {}
        for output_line in buck_test_output:
            matched = pyre_error_pattern.match(output_line)
            if matched:
                path = matched.group(1)
                line = int(matched.group(2))
                column = int(matched.group(3))
                description = matched.group(4)
                code = matched.group(5)
                error = {
                    "line": line,
                    "column": column,
                    "path": project_directory / path,
                    "code": code,
                    "description": description,
                    "concise_description": description,
                }
                errors[(line, column, path, code)] = error
        errors = Errors(list(errors.values()))
    elif check_alternate_names and buck_test.returncode == 5:
        # Generated type check target was not named as expected.
        LOG.warning("Could not find buck test targets: %s", targets)
        LOG.info("Looking for similar targets...")
        targets_to_retry = []
        for target in targets:
            query_command = ["buck", "query", target]
            similar_targets = subprocess.run(
                query_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
            )
            output = similar_targets.stdout.decode()
            error_output = similar_targets.stderr.decode()
            if output:
                targets_to_retry.append(output.strip())
            elif error_output:
                typecheck_targets = [
                    target.strip()
                    for target in error_output.split("\n")
                    if target.strip().endswith("-pyre-typecheck")
                ]
                targets_to_retry += typecheck_targets
        if targets_to_retry:
            LOG.info("Retrying similar targets: %s", targets_to_retry)
            errors = errors_from_targets(
                project_directory, path, targets_to_retry, check_alternate_names=False
            )
        else:
            LOG.error("No similar targets to retry.")
    else:
        LOG.error(
            "Failed to run buck test command:\n\t%s\n\n%s",
            " ".join(buck_test_command),
            buck_test.stderr.decode(),
        )
    return errors


def _remove_comment_preamble(lines: List[str]) -> None:
    # Deprecated: leaving remove logic until live old-style comments are cleaned up.
    while lines:
        old_line = lines.pop()
        new_line = re.sub(r"# pyre: .*$", "", old_line).rstrip()
        if old_line == "" or new_line != "":
            # The preamble has ended.
            lines.append(new_line)
            return


def _split_across_lines(
    comment: str, indent: int, max_line_length: Optional[int]
) -> List[str]:
    if not max_line_length or len(comment) <= max_line_length:
        return [comment]

    comment = comment.lstrip()
    available_columns = max_line_length - indent - len("#  ")

    buffered_line = ""
    result = []
    prefix = " " * indent
    for token in comment.split():
        if buffered_line and (
            len(buffered_line) + len(token) + len(" ") > available_columns
        ):
            # This new token would make the line exceed the limit,
            # hence terminate what we have accumulated.
            result.append(("{}{}".format(prefix, buffered_line)).rstrip())
            # The first line already has a comment token on it, so don't prefix #. For
            # the rest, we need to add the comment symbol manually.
            prefix = "{}#  ".format(" " * indent)
            buffered_line = ""

        buffered_line = buffered_line + token + " "

    result.append(("{}{}".format(prefix, buffered_line)).rstrip())
    return result


class SkippingGeneratedFileException(Exception):
    pass


def _suppress_errors(
    input: str,
    errors: Dict[int, List[Dict[str, str]]],
    custom_comment: Optional[str] = None,
    max_line_length: Optional[int] = None,
    truncate: bool = False,
    unsafe: bool = False,
) -> str:
    if "@" "generated" in input:
        raise SkippingGeneratedFileException()

    lines = input.split("\n")  # type: List[str]

    # Replace lines in file.
    new_lines = []
    removing_pyre_comments = False
    for index, line in enumerate(lines):
        if removing_pyre_comments:
            stripped = line.lstrip()
            if stripped.startswith("#") and not re.match(
                r"# *pyre-(ignore|fixme).*$", stripped
            ):
                continue
            else:
                removing_pyre_comments = False
        number = index + 1
        if number not in errors:
            new_lines.append(line)
            continue

        if any(error["code"] == "0" for error in errors[number]):
            # Handle unused ignores.
            replacement = re.sub(r"# pyre-(ignore|fixme).*$", "", line).rstrip()
            if replacement == "":
                removing_pyre_comments = True
                _remove_comment_preamble(new_lines)
                continue
            else:
                line = replacement

        comments = []
        for error in errors[number]:
            if error["code"] == "0":
                continue
            indent = len(line) - len(line.lstrip(" "))
            description = custom_comment if custom_comment else error["description"]
            comment = "{}# pyre-fixme[{}]: {}".format(
                " " * indent, error["code"], description
            )

            if not max_line_length or len(comment) <= max_line_length:
                comments.append(comment)
            else:
                truncated_comment = comment[: (max_line_length - 3)] + "..."
                split_comment_lines = _split_across_lines(
                    comment, indent, max_line_length
                )
                if truncate or len(split_comment_lines) > MAX_LINES_PER_FIXME:
                    comments.append(truncated_comment)
                else:
                    comments.extend(split_comment_lines)

        LOG.info(
            "Adding comment%s on line %d: %s",
            "s" if len(comments) > 1 else "",
            number,
            " \n".join(comments),
        )
        new_lines.extend(comments)
        new_lines.append(line)
    output = "\n".join(new_lines)
    if not unsafe:
        ast.check_stable(input, output)
    return output


def _build_error_map(
    errors: Iterator[Dict[str, Any]]
) -> Dict[int, List[Dict[str, str]]]:
    error_map = defaultdict(lambda: [])
    for error in errors:
        if error["concise_description"]:
            description = error["concise_description"]
        else:
            description = error["description"]
        match = re.search(r"\[(\d+)\]: (.*)", description)
        if match:
            error_map[error["line"]].append(
                {"code": match.group(1), "description": match.group(2)}
            )
    return error_map

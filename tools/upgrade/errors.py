# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import itertools
import json
import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Tuple

from .ast import verify_stable_ast
from .postprocess import LOG


def error_path(error: Dict[str, Any]) -> str:
    return error["path"]


class Errors:
    def __init__(self, errors: List[Dict[str, Any]]) -> None:
        self.errors: List[Dict[str, Any]] = errors
        self.error_iterator: Iterator[
            Tuple[str, Iterator[Dict[str, Any]]]
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

    @classmethod
    def empty(cls) -> "Errors":
        return cls([])


def _filter_errors(
    errors: List[Dict[str, Any]], only_fix_error_code: Optional[int] = None
) -> List[Dict[str, Any]]:
    if only_fix_error_code is not None:
        errors = [error for error in errors if error["code"] == only_fix_error_code]
    return errors


def json_to_errors(
    json_string: Optional[str], only_fix_error_code: Optional[int] = None
) -> Errors:
    if json_string:
        try:
            errors = json.loads(json_string)
            return Errors(_filter_errors(errors, only_fix_error_code))
        except json.decoder.JSONDecodeError:
            LOG.error(
                "Recevied invalid JSON as input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
    else:
        LOG.error(
            "Recevied no input."
            "If piping from `pyre check` be sure to use `--output=json`."
        )
    return Errors.empty()


def errors_from_stdin(only_fix_error_code: Optional[int] = None) -> Errors:
    input = sys.stdin.read()
    return json_to_errors(input)


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
        LOG.info("No errors in %s/TARGETS...", path)
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
                targets_to_retry.append(output)
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


@verify_stable_ast
def _fix_file(
    filename: str,
    errors: Dict[int, List[Dict[str, str]]],
    custom_comment: Optional[str] = None,
    max_line_length: Optional[int] = None,
    truncate: bool = False,
) -> None:
    path = Path(filename)
    text = path.read_text()
    if "@" "generated" in text:
        LOG.warning("Attempting to upgrade generated file %s, skipping.", filename)
        return
    lines = text.split("\n")  # type: List[str]

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
        if errors[number][0]["code"] == "0":
            # Handle unused ignores.
            removing_pyre_comments = True
            replacement = re.sub(r"# pyre-(ignore|fixme).*$", "", line).rstrip()
            if replacement == "":
                _remove_comment_preamble(new_lines)
            else:
                new_lines.append(replacement)
            continue

        comments = []
        for error in errors[number]:
            indent = len(line) - len(line.lstrip(" "))
            description = custom_comment if custom_comment else error["description"]
            comment = "{}# pyre-fixme[{}]: {}".format(
                " " * indent, error["code"], description
            )

            if not max_line_length or len(comment) <= max_line_length:
                comments.append(comment)
            else:
                if truncate:
                    comments.append(comment[: (max_line_length - 3)] + "...")
                else:
                    comments.extend(
                        _split_across_lines(comment, indent, max_line_length)
                    )

        LOG.info(
            "Adding comment%s on line %d: %s",
            "s" if len(comments) > 1 else "",
            number,
            " \n".join(comments),
        )
        new_lines.extend(comments)
        new_lines.append(line)
    new_text = "\n".join(new_lines)
    path.write_text(new_text)


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


def fix(
    errors: Errors, comment: str = "", max_line_length: int = 0, truncate: bool = False
) -> None:
    for path, errors in errors:
        LOG.info("Processing `%s`", path)
        _fix_file(
            path,
            _build_error_map(errors),
            comment,
            max_line_length if max_line_length > 0 else None,
            truncate,
        )

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import itertools
import json
import re
import sys
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Tuple

from .ast import verify_stable_ast
from .postprocess import LOG


def json_to_errors(json_string: Optional[str]) -> List[Dict[str, Any]]:
    if json_string:
        try:
            return json.loads(json_string)
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
    return []


def sort_errors(
    errors: List[Dict[str, Any]]
) -> Iterator[Tuple[str, Iterator[Dict[str, Any]]]]:
    def error_path(error: Dict[str, Any]) -> str:
        return error["path"]

    return itertools.groupby(sorted(errors, key=error_path), error_path)


def filter_errors(
    arguments: argparse.Namespace, errors: List[Dict[str, Any]]
) -> List[Dict[str, Any]]:
    only_fix_error_code: Optional[int] = arguments.only_fix_error_code
    if only_fix_error_code is not None:
        errors = [error for error in errors if error["code"] == only_fix_error_code]
    return errors


def errors_from_stdin(_arguments: argparse.Namespace) -> List[Dict[str, Any]]:
    input = sys.stdin.read()
    errors = json_to_errors(input)
    return filter_errors(_arguments, errors)


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
def fix_file(
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
            if line.startswith("#") and not re.match(
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

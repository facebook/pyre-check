# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import itertools
import json
import logging
import pathlib
import re
import sys
import traceback
from collections import defaultdict
from typing import Any, List, Tuple

from .commands import ExitCode


LOG = logging.getLogger(__name__)


def run_fixme(arguments, result) -> None:
    for path, errors in result:
        LOG.info("Processing `%s`", path)

        # Build map from line to error codes.
        codes = defaultdict(lambda: set())
        descriptions = defaultdict(lambda: set())
        for error in errors:
            match = re.search(r"\[(\d+)\]: (.*)", error["description"])
            if match:
                codes[error["line"]].add(match.group(1))
                descriptions[error["line"]].add(match.group(2))

        # Replace lines in file.
        path = pathlib.Path(path)
        lines = path.read_text().split("\n")

        new_lines = []
        for index, line in enumerate(lines):
            number = index + 1
            if number in codes:
                if list(codes[number]) == ["0"]:
                    # Handle unused ignores.
                    replacement = re.sub(r"# pyre-(ignore|fixme).*$", "", line).rstrip()
                    if replacement != "":
                        new_lines.append(replacement)
                    continue

                sorted_codes = sorted(list(codes[number]))
                sorted_descriptions = sorted(list(descriptions[number]))

                description = ""
                if hasattr(arguments, "comment") and arguments.comment:
                    description = ": " + arguments.comment
                else:
                    description = ": " + ", ".join(sorted_descriptions)

                comment = "{}# pyre-fixme[{}]{}".format(
                    line[: (len(line) - len(line.lstrip(" ")))],  # indent
                    ", ".join([str(code) for code in sorted_codes]),
                    description,
                )
                LOG.info("Adding `%s` on line %d", comment, number)

                new_lines.extend([comment, line])
            else:
                new_lines.append(line)

        path.write_text("\n".join(new_lines))


def run_missing_overridden_return_annotations(
    arugments, errors: List[Tuple[str, List[Any]]]
) -> None:
    for path, errors in result:
        LOG.info("Patching errors in `%s`.", path)
        errors = reversed(sorted(errors, key=lambda error: error["line"]))

        path = pathlib.Path(path)
        lines = path.read_text().split("\n")

        for error in errors:
            if error["code"] != 15:
                continue
            line = error["line"] - 1

            match = re.match(r".*`(.*)`\.", error["description"])
            if not match:
                continue
            annotation = match.groups()[0]

            # Find last closing parenthesis in after line.
            LOG.info("Looking at %d: %s", line, lines[line])
            while True:
                if "):" in lines[line]:
                    lines[line] = lines[line].replace("):", ") -> %s:" % annotation)
                    LOG.info("%d: %s", line, lines[line])
                    break
                else:
                    line = line + 1

        LOG.warn("Writing patched %s", str(path))
        path.write_text("\n".join(lines))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")

    # Subcommands.
    commands = parser.add_subparsers()

    fixme = commands.add_parser("fixme")
    fixme.set_defaults(function=run_fixme)
    fixme.add_argument("--comment", help="Custom comment after fixme comments")

    missing_overridden_return_annotations = commands.add_parser(
        "missing-overridden-return-annotations"
    )
    missing_overridden_return_annotations.set_defaults(
        function=run_missing_overridden_return_annotations
    )

    # Initialize default values.
    arguments = parser.parse_args()
    if not hasattr(arguments, "function"):
        arguments.function = run_fixme

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    try:
        exit_code = ExitCode.SUCCESS

        def error_path(error):
            return error["path"]

        input_string = sys.stdin.read()
        errors = json.loads(input_string) if input_string else json.loads("{}")
        result = itertools.groupby(sorted(errors, key=error_path), error_path)
        arguments.function(arguments, result)
    except json.decoder.JSONDecodeError:
        if not input_string:
            LOG.error(
                "Recevied no input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
        else:
            LOG.error(
                "Recevied invalid JSON as input."
                "If piping from `pyre check` be sure to use `--output=json`."
            )
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = ExitCode.FAILURE

    sys.exit(exit_code)

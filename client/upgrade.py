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

from .commands import ExitCode


LOG = logging.getLogger(__name__)


class PostprocessError(Exception):
    pass


def run_fixme(arguments) -> None:
    try:

        def error_path(error):
            return error["path"]

        result = itertools.groupby(
            sorted(json.load(sys.stdin), key=error_path), error_path
        )

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
                        replacement = re.sub(
                            r"# pyre-(ignore|fixme).*$", "", line
                        ).rstrip()
                        if replacement != "":
                            new_lines.append(replacement)
                        continue

                    sorted_codes = sorted(list(codes[number]))
                    sorted_descriptions = sorted(list(descriptions[number]))

                    description = ""
                    if arguments.comment:
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
    except (json.JSONDecodeError, OSError) as error:
        raise PostprocessError(str(error))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")

    # Subcommands.
    commands = parser.add_subparsers()

    fixme = commands.add_parser("fixme")
    fixme.set_defaults(function=run_fixme)
    fixme.add_argument("--comment", help="Custom comment after fixme comments")

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
        arguments.function(arguments)
    except PostprocessError as error:
        LOG.error(str(error))
        LOG.debug(traceback.format_exc())
        exit_code = ExitCode.FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = ExitCode.FAILURE

    sys.exit(exit_code)

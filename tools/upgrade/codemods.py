# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import pathlib
import re
from logging import Logger

from .errors import errors_from_stdin, sort_errors


LOG: Logger = logging.getLogger(__name__)


def run_missing_overridden_return_annotations(
    arguments: argparse.Namespace, _version_control
) -> None:
    errors = sort_errors(errors_from_stdin(arguments))
    for path, errors in errors:
        LOG.info("Patching errors in `%s`.", path)
        errors = sorted(errors, key=lambda error: error["line"], reverse=True)

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


def run_missing_global_annotations(
    arguments: argparse.Namespace, _version_control
) -> None:
    errors = sort_errors(errors_from_stdin(arguments))
    for path, errors in errors:
        LOG.info("Patching errors in `%s`", path)
        errors = sorted(errors, key=lambda error: error["line"], reverse=True)

        path = pathlib.Path(path)
        lines = path.read_text().split("\n")

        for error in errors:
            if error["code"] != 5:
                continue
            line = error["line"] - 1

            match = re.match(r".*`.*`.*`(.*)`.*", error["description"])
            if not match:
                continue
            annotation = match.groups()[0]

            LOG.info("Looking at %d: %s", line, lines[line])
            if " =" in lines[line]:
                lines[line] = lines[line].replace(" =", ": %s =" % annotation)
                LOG.info("%d: %s", line, lines[line])

        path.write_text("\n".join(lines))

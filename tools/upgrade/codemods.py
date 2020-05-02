# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import pathlib
import re
from logging import Logger

from .command import Command
from .errors import errors_from_stdin


LOG: Logger = logging.getLogger(__name__)


class MissingOverrideReturnAnnotations(Command):
    def run(self) -> None:
        errors = errors_from_stdin(self._arguments.only_fix_error_code)
        for path, errors in errors:
            LOG.info("Patching errors in `%s`.", path)
            errors = sorted(errors, key=lambda error: error["line"], reverse=True)

            # pyre-fixme[6]: Expected `Union[_PathLike[str], str]` for 1st param but got
            #  `Union[typing.Iterator[typing.Dict[str, typing.Any]], str]`.
            path = pathlib.Path(path)
            lines = path.read_text().split("\n")

            for error in errors:
                # pyre-fixme[6]: Expected `Union[int, slice]` for 1st param but got
                # `str`.
                if error["code"] != 15:
                    continue
                # pyre-fixme[6]: Expected `int` for 1st param but got `str`.
                # pyre-fixme[6]: Expected `Union[int, slice]` for 1st param but got
                # `str`.
                line = error["line"] - 1

                # pyre-fixme[6]: Expected `Union[int, slice]` for 1st param but got
                # `str`.
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


class MissingGlobalAnnotations(Command):
    def run(self) -> None:
        errors = errors_from_stdin(self._arguments.only_fix_error_code)
        for path, errors in errors:
            LOG.info("Patching errors in `%s`", path)
            errors = sorted(errors, key=lambda error: error["line"], reverse=True)

            # pyre-fixme[6]: Expected `Union[_PathLike[str], str]` for 1st param but got
            #  `Union[typing.Iterator[typing.Dict[str, typing.Any]], str]`.
            path = pathlib.Path(path)
            lines = path.read_text().split("\n")

            for error in errors:
                # pyre-fixme[6]: Expected `Union[int, slice]` for 1st param but got
                # `str`.
                if error["code"] != 5:
                    continue
                # pyre-fixme[6]: Expected `int` for 1st param but got `str`.
                # pyre-fixme[6]: Expected `Union[int, slice]` for 1st param but got
                # `str`.
                line = error["line"] - 1

                # pyre-fixme[6]: Expected `Union[int, slice]` for 1st param but got
                # `str`.
                match = re.match(r".*`.*`.*`(.*)`.*", error["description"])
                if not match:
                    continue
                annotation = match.groups()[0]

                LOG.info("Looking at %d: %s", line, lines[line])
                if " =" in lines[line]:
                    lines[line] = lines[line].replace(" =", ": %s =" % annotation)
                    LOG.info("%d: %s", line, lines[line])

            path.write_text("\n".join(lines))

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import pathlib
import re
from logging import Logger
from pathlib import Path

from ..commands.command import Command
from ..configuration import Configuration
from ..errors import Errors
from ..filesystem import path_exists
from ..repository import Repository


LOG: Logger = logging.getLogger(__name__)


class MissingOverrideReturnAnnotations(Command):
    def __init__(self, *, repository: Repository, only_fix_error_code: int) -> None:
        super().__init__(repository)
        self._only_fix_error_code: int = only_fix_error_code

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "MissingOverrideReturnAnnotations":
        return MissingOverrideReturnAnnotations(
            repository=repository, only_fix_error_code=arguments.only_fix_error_code
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(MissingOverrideReturnAnnotations, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "--only-fix-error-code",
            type=int,
            help="Only add fixmes for errors with this specific error code.",
            default=None,
        )

    def run(self) -> None:
        errors = Errors.from_stdin(self._only_fix_error_code)
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
    def __init__(self, *, repository: Repository, only_fix_error_code: int) -> None:
        super().__init__(repository)
        self._only_fix_error_code: int = only_fix_error_code

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "MissingGlobalAnnotations":
        return MissingGlobalAnnotations(
            repository=repository, only_fix_error_code=arguments.only_fix_error_code
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(MissingGlobalAnnotations, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "--only-fix-error-code",
            type=int,
            help="Only add fixmes for errors with this specific error code.",
            default=None,
        )

    def run(self) -> None:
        errors = Errors.from_stdin(self._only_fix_error_code)
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


class EnableSourceDatabaseBuckBuilder(Command):
    def __init__(self, *, local_root: Path, repository: Repository) -> None:
        super().__init__(repository)
        self._local_root = local_root

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "EnableSourceDatabaseBuckBuilder":
        return EnableSourceDatabaseBuckBuilder(
            local_root=arguments.local_root, repository=repository
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(EnableSourceDatabaseBuckBuilder, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "local_root",
            help="Path to directory with local configuration",
            type=path_exists,
        )

    def run(self) -> None:
        configuration = Configuration(self._local_root / ".pyre_configuration.local")
        configuration.enable_source_database_buck_builder()
        configuration.write()

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import pathlib
import re
from logging import Logger
from pathlib import Path
from typing import Sequence

from pyre_extensions import override

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

    @override
    def run(self) -> None:
        errors = Errors.from_stdin(self._only_fix_error_code)
        # pyre-fixme[16]: `List` has no attribute `paths_to_errors`.
        for path, errors in errors.paths_to_errors.items():
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

    @override
    def run(self) -> None:
        errors = Errors.from_stdin(self._only_fix_error_code)
        # pyre-fixme[16]: `List` has no attribute `paths_to_errors`.
        for path, errors in errors.paths_to_errors.items():
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


class SetUseBuck1(Command):
    def __init__(self, *, local_roots: Sequence[Path], repository: Repository) -> None:
        super().__init__(repository)
        self._local_roots = local_roots

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "SetUseBuck1":
        return SetUseBuck1(local_roots=arguments.local_roots, repository=repository)

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(SetUseBuck1, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "local_roots",
            help="Paths to directory with local configuration",
            type=path_exists,
            nargs="*",
        )

    @override
    def run(self) -> None:
        for local_root in self._local_roots:
            configuration = Configuration(local_root / ".pyre_configuration.local")
            configuration.set_use_buck1_if_possible()
            configuration.write()

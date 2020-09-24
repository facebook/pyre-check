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
from typing import Sequence

import libcst
from libcst.codemod import CodemodContext
from libcst.codemod.visitors import AddImportsVisitor

from ..commands.command import (
    Command,
    CommandArguments,
    ErrorSource,
    ProjectErrorSuppressingCommand,
)
from ..configuration import Configuration
from ..errors import Errors, UserError
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
    def __init__(self, *, local_roots: Sequence[Path], repository: Repository) -> None:
        super().__init__(repository)
        self._local_roots = local_roots

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "EnableSourceDatabaseBuckBuilder":
        return EnableSourceDatabaseBuckBuilder(
            local_roots=arguments.local_roots, repository=repository
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(EnableSourceDatabaseBuckBuilder, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "local_roots",
            help="Path to directory with local configuration",
            type=path_exists,
            nargs="*",
        )

    def run(self) -> None:
        for local_root in self._local_roots:
            configuration = Configuration(local_root / ".pyre_configuration.local")
            configuration.enable_source_database_buck_builder()
            configuration.write()


class SupportSqlalchemy(ProjectErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        local_root: Path,
        path: Path,
        repository: Repository
    ) -> None:
        super().__init__(
            command_arguments,
            repository=repository,
            only_fix_error_code=None,
            upgrade_version=False,
            error_source=ErrorSource.GENERATE.value,
            no_commit=True,
            submit=False,
        )
        self._local_root = local_root
        self._path = path

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "SupportSqlalchemy":
        command_arguments = CommandArguments.from_arguments(arguments)
        return SupportSqlalchemy(
            command_arguments=command_arguments,
            local_root=arguments.local_root,
            path=arguments.path,
            repository=repository,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(SupportSqlalchemy, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "-l",
            "--local-root",
            help="Path to directory with local configuration",
            type=path_exists,
            required=True,
        )
        parser.add_argument(
            "path", help="Path to file using sqlalchemy", type=path_exists
        )

    def _annotate_sqlalchemy_files(self, configuration: Configuration) -> None:
        pyre_output = configuration.run_pyre(
            arguments=[
                "--strict",
                "-l",
                str(self._local_root),
                "--noninteractive",
                "infer",
                "--in-place",
                str(self._path),
            ],
            description="Running `pyre infer`",
            should_clean=True,
            stderr_flag=None,
        )
        if pyre_output is None:
            raise UserError("Couldn't annotate sqlalchemy files.")

    def _import_annotations_from_future(self) -> None:
        """We need this because the original sqlalchemy types aren't generic
        and will fail at runtime."""
        LOG.info("Importing necessary annotations...")
        context = CodemodContext()
        AddImportsVisitor.add_needed_import(context, "__future__", "annotations")
        source = libcst.parse_module(self._path.read_text())
        modified_tree = AddImportsVisitor(context).transform_module(source)
        self._path.write_text(modified_tree.code)

    def run(self) -> None:
        local_configuration_path = self._local_root / ".pyre_configuration.local"
        local_configuration = Configuration(local_configuration_path)
        self._annotate_sqlalchemy_files(local_configuration)
        self._import_annotations_from_future()

        project_configuration = Configuration.find_project_configuration()
        self._suppress_errors_in_project(
            local_configuration, project_configuration.parent
        )

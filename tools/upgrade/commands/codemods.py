# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import functools
import json
import logging
import pathlib
import re
from logging import Logger
from pathlib import Path
from typing import Dict, List, Optional, Sequence

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
from ..errors import Errors, PathsToErrors, PyreError, UserError
from ..filesystem import find_files, path_exists
from ..repository import Repository


LOG: Logger = logging.getLogger(__name__)


MISSING_ATTRIBUTE_ANNOTATION_ERROR_CODE = 4


def _is_sqlalchemy_error(error: PyreError) -> bool:
    return bool(
        re.search(
            r"has type .*?Column\[.*but no type is specified.", error["description"]
        )
    )


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

    def run(self) -> None:
        errors = Errors.from_stdin(self._only_fix_error_code)
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
        paths: Sequence[Path],
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
        self._paths: Optional[Sequence[Path]] = paths if len(paths) > 0 else None

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "SupportSqlalchemy":
        command_arguments = CommandArguments.from_arguments(arguments)
        return SupportSqlalchemy(
            command_arguments=command_arguments,
            local_root=arguments.local_root,
            paths=arguments.paths,
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
            "paths", help="Paths using sqlalchemy", type=path_exists, nargs="*"
        )

    @staticmethod
    def _get_sqlalchemy_errors(
        path_wise_errors: Dict[str, List[PyreError]],
        filter_paths: Optional[Sequence[Path]],
    ) -> PathsToErrors:
        all_pathwise_sqlalchemy_errors = {
            Path(pathname): [error for error in errors if _is_sqlalchemy_error(error)]
            for pathname, errors in path_wise_errors.items()
        }

        nonempty_pathwise_sqlalchemy_errors = {
            path: errors
            for path, errors in all_pathwise_sqlalchemy_errors.items()
            if len(errors) > 0
        }

        if filter_paths is None:
            return nonempty_pathwise_sqlalchemy_errors

        return {
            path: errors
            for path, errors in nonempty_pathwise_sqlalchemy_errors.items()
            if path in filter_paths
        }

    def _annotate_sqlalchemy_files(
        self, configuration: Configuration, sqlalchemy_path_wise_errors: PathsToErrors
    ) -> None:
        paths = [str(path) for path in sqlalchemy_path_wise_errors.keys()]
        errors = [
            error for errors in sqlalchemy_path_wise_errors.values() for error in errors
        ]
        pyre_output = configuration.run_pyre(
            arguments=[
                "--strict",
                "-l",
                str(self._local_root),
                "--noninteractive",
                "infer",
                "--json",
                "--in-place",
                *paths,
            ],
            description="Running `pyre infer`",
            should_clean=True,
            stderr_flag=None,
            command_input=json.dumps(errors),
        )
        if pyre_output is None:
            raise UserError("Couldn't annotate sqlalchemy files.")

    def _import_annotations_from_future(
        self, sqlalchemy_path_wise_errors: PathsToErrors
    ) -> None:
        """We need this because the original sqlalchemy types aren't generic
        and will fail at runtime."""
        LOG.info("Importing necessary annotations...")
        context = CodemodContext()
        AddImportsVisitor.add_needed_import(context, "__future__", "annotations")
        paths = list(sqlalchemy_path_wise_errors.keys())
        for path in paths:
            source = libcst.parse_module(path.read_text())
            modified_tree = AddImportsVisitor(context).transform_module(source)
            path.write_text(modified_tree.code)

    def run(self) -> None:
        local_configuration_path = self._local_root / ".pyre_configuration.local"
        local_configuration = Configuration(local_configuration_path)

        unannotated_attribute_errors = local_configuration.get_errors(
            only_fix_error_code=MISSING_ATTRIBUTE_ANNOTATION_ERROR_CODE, strict=True
        )

        sqlalchemy_path_wise_errors = self._get_sqlalchemy_errors(
            unannotated_attribute_errors.paths_to_errors, self._paths
        )

        if len(sqlalchemy_path_wise_errors) == 0:
            LOG.warning("No paths with missing annotations. Exiting...")
            return

        LOG.info("Found errors: %s", sqlalchemy_path_wise_errors)

        LOG.info(
            "Annotating the following sqlalchemy files: `%s`",
            list(sqlalchemy_path_wise_errors.keys()),
        )
        self._annotate_sqlalchemy_files(
            local_configuration, sqlalchemy_path_wise_errors
        )
        self._import_annotations_from_future(sqlalchemy_path_wise_errors)

        project_configuration = Configuration.find_project_configuration()
        self._suppress_errors_in_project(
            local_configuration, project_configuration.parent
        )

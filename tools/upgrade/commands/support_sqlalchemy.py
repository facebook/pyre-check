# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import re
from logging import Logger
from pathlib import Path
from typing import Dict, List, Optional, Sequence

import libcst
from libcst.codemod import CodemodContext
from libcst.codemod.visitors import AddImportsVisitor
from pyre_extensions import override

from ..commands.command import CommandArguments, ErrorSuppressingCommand
from ..configuration import Configuration
from ..errors import PathsToErrors, PyreError, UserError
from ..filesystem import path_exists
from ..repository import Repository


LOG: Logger = logging.getLogger(__name__)


MISSING_ATTRIBUTE_ANNOTATION_ERROR_CODE = 4
UNBOUND_NAME_ERROR_CODE = 10


def _is_sqlalchemy_error(error: PyreError) -> bool:
    return bool(
        re.search(
            r"has type .*?Column\[.*but no type is specified.", error["description"]
        )
    )


class SupportSqlalchemy(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        local_root: Path,
        paths: Sequence[Path],
        repository: Repository,
    ) -> None:
        super().__init__(command_arguments, repository=repository)
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
        pyre_output = configuration.run_pyre(
            arguments=[
                "--strict",
                "-l",
                str(self._local_root),
                "--noninteractive",
                "infer-v2",
                "--in-place",
                "--dequalify",
                *paths,
            ],
            description="Running `pyre infer-v2`",
            should_clean=self._should_clean,
            stderr_flag=None,
            command_input=None,
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

    @override
    def run(self) -> None:
        local_configuration_path = self._local_root / ".pyre_configuration.local"
        local_configuration = Configuration(local_configuration_path)

        unannotated_attribute_errors = local_configuration.get_errors(
            only_fix_error_code=MISSING_ATTRIBUTE_ANNOTATION_ERROR_CODE,
            strict=True,
            should_clean=self._should_clean,
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

        self._get_and_suppress_errors(local_configuration)

        title = "Suppress errors for {}".format(self._local_root)
        self._repository.commit_changes(commit=(not self._no_commit), title=title)

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from dataclasses import dataclass
from enum import Enum
from typing import Optional

from ..configuration import Configuration
from ..errors import Errors, PartialErrorSuppression
from ..filesystem import add_local_mode, LocalMode
from ..repository import Repository


LOG: logging.Logger = logging.getLogger(__name__)


class ErrorSource(Enum):
    STDIN = "stdin"
    GENERATE = "generate"

    def __repr__(self) -> str:
        return self.value


@dataclass(frozen=True)
class CommandArguments:
    comment: Optional[str]
    max_line_length: Optional[int]
    truncate: bool
    unsafe: bool
    force_format_unsuppressed: bool
    lint: bool
    no_commit: bool
    should_clean: bool

    @staticmethod
    def from_arguments(arguments: argparse.Namespace) -> "CommandArguments":
        return CommandArguments(
            comment=arguments.comment,
            max_line_length=arguments.max_line_length,
            truncate=arguments.truncate,
            unsafe=getattr(arguments, "unsafe", False),
            force_format_unsuppressed=getattr(
                arguments, "force_format_unsuppressed", False
            ),
            lint=arguments.lint,
            no_commit=arguments.no_commit,
            should_clean=arguments.should_clean,
        )


class Command:
    def __init__(self, repository: Repository) -> None:
        self._repository: Repository = repository

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        pass

    def run(self) -> None:
        pass


class ErrorSuppressingCommand(Command):
    def __init__(
        self, command_arguments: CommandArguments, repository: Repository
    ) -> None:
        super().__init__(repository)
        self._command_arguments: CommandArguments = command_arguments
        self._comment: Optional[str] = command_arguments.comment
        self._max_line_length: Optional[int] = command_arguments.max_line_length
        self._truncate: bool = command_arguments.truncate
        self._unsafe: bool = command_arguments.unsafe
        self._force_format_unsuppressed: bool = (
            command_arguments.force_format_unsuppressed
        )
        self._lint: bool = command_arguments.lint
        self._no_commit: bool = command_arguments.no_commit
        self._should_clean: bool = command_arguments.should_clean

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        super(ErrorSuppressingCommand, ErrorSuppressingCommand).add_arguments(parser)
        parser.add_argument("--comment", help="Custom comment after fixme comments")
        parser.add_argument(
            "--max-line-length",
            default=88,
            type=int,
            help="Enforce maximum line length on new comments "
            + "(default: %(default)s, use 0 to set no maximum line length)",
        )
        parser.add_argument(
            "--truncate",
            action="store_true",
            help="Truncate error messages to maximum line length.",
        )
        parser.add_argument(
            "--unsafe",
            action="store_true",
            help="Don't check syntax when applying fixmes.",
        )
        parser.add_argument(
            "--force-format-unsuppressed", action="store_true", help=argparse.SUPPRESS
        )
        parser.add_argument(
            "--lint",
            action="store_true",
            help="Run lint to ensure added fixmes comply with black formatting. \
            Doubles the runtime of pyre-ugprade.",
        )
        parser.add_argument("--no-commit", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument(
            "--do-not-run-buck-clean",
            action="store_false",
            dest="should_clean",
            default=True,
            help=argparse.SUPPRESS,
        )

    def _apply_suppressions(self, errors: Errors) -> None:
        try:
            errors.suppress(
                self._comment,
                self._max_line_length,
                self._truncate,
                self._unsafe,
            )
        except PartialErrorSuppression as partial_error_suppression:
            if not self._force_format_unsuppressed:
                raise partial_error_suppression
            self._repository.force_format(partial_error_suppression.unsuppressed_paths)
            errors.suppress(
                self._comment,
                self._max_line_length,
                self._truncate,
                self._unsafe,
            )

    def _get_and_suppress_errors(
        self,
        configuration: Configuration,
        error_source: ErrorSource = ErrorSource.GENERATE,
        upgrade_version: bool = False,
        only_fix_error_code: Optional[int] = None,
        fixme_threshold: Optional[int] = None,
        fixme_threshold_fallback_mode: LocalMode = LocalMode.IGNORE,
    ) -> None:
        LOG.info("Processing %s", configuration.get_directory())
        if not configuration.is_local:
            return
        if upgrade_version:
            if configuration.version:
                configuration.remove_version()
                configuration.write()
            else:
                return
        errors = (
            Errors.from_stdin(only_fix_error_code)
            if error_source == ErrorSource.STDIN and not upgrade_version
            else configuration.get_errors(
                only_fix_error_code, should_clean=self._should_clean
            )
        )
        if len(errors) == 0:
            return

        if fixme_threshold is None:
            self._apply_suppressions(errors)
        else:
            for path, path_errors in errors.paths_to_errors.items():
                path_errors = list(path_errors)
                if len(path_errors) > fixme_threshold:
                    LOG.info(
                        "%d errors found in `%s`. Adding file-level ignore.",
                        len(path_errors),
                        path,
                    )
                    add_local_mode(path, fixme_threshold_fallback_mode)
                else:
                    self._apply_suppressions(Errors(path_errors))

        # Lint and re-run pyre once to resolve most formatting issues
        if self._lint and self._repository.format():
            errors = configuration.get_errors(only_fix_error_code, should_clean=False)
            self._apply_suppressions(errors)

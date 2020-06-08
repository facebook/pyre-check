# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from pathlib import Path

from ..configuration import Configuration
from ..errors import Errors, PartialErrorSuppression
from ..repository import Repository


LOG: logging.Logger = logging.getLogger(__name__)


class Command:
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        self._arguments: argparse.Namespace = arguments
        self._repository: Repository = repository

    @staticmethod
    def add_arguments(parser: argparse.ArgumentParser) -> None:
        pass

    def run(self) -> None:
        pass


class ErrorSuppressingCommand(Command):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._comment: str = arguments.comment
        self._max_line_length: int = arguments.max_line_length
        self._truncate: bool = arguments.truncate
        self._unsafe: bool = getattr(arguments, "unsafe", False)
        self._force_format_unsuppressed: bool = getattr(
            arguments, "force_format_unsuppressed", False
        )

    @staticmethod
    def add_arguments(parser: argparse.ArgumentParser) -> None:
        Command.add_arguments(parser)
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

    def _suppress_errors(self, errors: Errors) -> None:
        try:
            errors.suppress(
                self._comment, self._max_line_length, self._truncate, self._unsafe
            )
        except PartialErrorSuppression as partial_error_suppression:
            if not self._force_format_unsuppressed:
                raise partial_error_suppression
            self._repository.force_format(partial_error_suppression.unsuppressed_paths)
            errors.suppress(
                self._comment, self._max_line_length, self._truncate, self._unsafe
            )


class ProjectErrorSuppressingCommand(ErrorSuppressingCommand):
    def __init__(self, arguments: argparse.Namespace, repository: Repository) -> None:
        super().__init__(arguments, repository)
        self._only_fix_error_code: int = arguments.only_fix_error_code
        self._upgrade_version: bool = arguments.upgrade_version
        self._error_source: str = arguments.error_source
        self._lint: bool = arguments.lint
        self._no_commit: bool = arguments.no_commit
        self._submit: bool = arguments.submit

    @staticmethod
    def add_arguments(parser: argparse.ArgumentParser) -> None:
        ErrorSuppressingCommand.add_arguments(parser)
        parser.add_argument(
            "--only-fix-error-code",
            type=int,
            help="Only add fixmes for errors with this specific error code.",
            default=None,
        )
        parser.add_argument(
            "--upgrade-version",
            action="store_true",
            help="Upgrade and clean project if a version override set.",
        )
        parser.add_argument(
            "--error-source", choices=["stdin", "generate"], default="generate"
        )
        parser.add_argument("--lint", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument("--no-commit", action="store_true", help=argparse.SUPPRESS)
        parser.add_argument("--submit", action="store_true", help=argparse.SUPPRESS)

    def _suppress_errors_in_project(
        self, configuration: Configuration, root: Path
    ) -> None:
        LOG.info("Processing %s", configuration.get_directory())
        if not configuration.is_local:
            return
        if self._upgrade_version:
            if configuration.version:
                configuration.remove_version()
                configuration.write()
            else:
                return
        errors = (
            Errors.from_stdin(self._only_fix_error_code)
            if self._error_source == "stdin" and not self._upgrade_version
            else configuration.get_errors()
        )
        if len(errors) > 0:
            self._suppress_errors(errors)

            # Lint and re-run pyre once to resolve most formatting issues
            if self._lint:
                if self._repository.format():
                    errors = configuration.get_errors(should_clean=False)
                    self._suppress_errors(errors)

        project_root = root.resolve()
        local_root = configuration.get_directory().resolve()
        title = "{} for {}".format(
            "Update pyre version" if self._upgrade_version else "Suppress pyre errors",
            str(local_root.relative_to(project_root)),
        )
        self._repository.submit_changes(
            commit=(not self._no_commit), submit=self._submit, title=title
        )

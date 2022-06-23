# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
from typing import Optional

from pyre_extensions import override

from ..configuration import Configuration
from ..errors import Errors
from ..repository import Repository
from .command import CommandArguments, ErrorSource, ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


class Fixme(ErrorSuppressingCommand):
    def __init__(
        self,
        command_arguments: CommandArguments,
        *,
        repository: Repository,
        error_source: str,
        only_fix_error_code: Optional[int] = None,
    ) -> None:
        super().__init__(command_arguments, repository)
        self._error_source: str = error_source
        self._only_fix_error_code: Optional[int] = only_fix_error_code

    @staticmethod
    def from_arguments(
        arguments: argparse.Namespace, repository: Repository
    ) -> "Fixme":
        command_arguments = CommandArguments.from_arguments(arguments)
        return Fixme(
            command_arguments,
            repository=repository,
            error_source=arguments.error_source,
            only_fix_error_code=arguments.only_fix_error_code,
        )

    @classmethod
    def add_arguments(cls, parser: argparse.ArgumentParser) -> None:
        # Make this compatible with `fixme-single`.
        super(Fixme, cls).add_arguments(parser)
        parser.set_defaults(command=cls.from_arguments)
        parser.add_argument(
            "--error-source",
            type=ErrorSource,
            choices=list(ErrorSource),
            default=ErrorSource.STDIN,
        )
        parser.add_argument(
            "--only-fix-error-code",
            type=int,
            help="Only add fixmes for errors with this specific error code.",
            default=None,
        )

    @override
    def run(self) -> None:
        if self._error_source == ErrorSource.GENERATE:
            errors = self._generate_errors()
            self._apply_suppressions(errors)

            if self._lint:
                if self._repository.format():
                    errors = self._generate_errors()
                    self._apply_suppressions(errors)
        else:
            errors = Errors.from_stdin(self._only_fix_error_code)
            self._apply_suppressions(errors)

    def _generate_errors(self) -> Errors:
        configuration_path = Configuration.find_project_configuration()
        configuration = Configuration(configuration_path)
        return configuration.get_errors(self._only_fix_error_code)

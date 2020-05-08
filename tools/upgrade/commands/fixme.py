# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
from typing import Optional

from ..configuration import Configuration
from ..errors import Errors
from .command import ErrorSuppressingCommand


LOG: logging.Logger = logging.getLogger(__name__)


def _errors_from_run(only_fix_error_code: Optional[int] = None) -> Errors:
    configuration_path = Configuration.find_project_configuration()
    if not configuration_path:
        LOG.warning("Could not find pyre configuration.")
        return Errors.empty()
    with open(configuration_path) as configuration_file:
        configuration = Configuration(configuration_path, json.load(configuration_file))
        return configuration.get_errors(only_fix_error_code)


class Fixme(ErrorSuppressingCommand):
    def run(self) -> None:
        # Suppress errors in project with no local configurations.
        if self._arguments.error_source == "generate":
            errors = _errors_from_run(self._arguments.only_fix_error_code)
            self._suppress_errors(errors)

            if self._arguments.lint:
                if self._repository.format():
                    errors = _errors_from_run(self._arguments.only_fix_error_code)
                    self._suppress_errors(errors)
        else:
            errors = Errors.from_stdin(self._arguments.only_fix_error_code)
            self._suppress_errors(errors)

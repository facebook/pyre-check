# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from .. import FOUND_ERRORS
from .error_handling import ErrorHandling


class Check(ErrorHandling):
    NAME = "check"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Check, self).__init__(arguments, configuration, analysis_directory)
        self._log_identifier = arguments.log_identifier
        self._number_of_workers = configuration.number_of_workers
        self._logger = arguments.logger or configuration.logger

    def _flags(self):
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        if len(filter_directories):
            flags.extend(
                ["-filter-directories-semicolon", ";".join(list(filter_directories))]
            )
        flags.extend(
            [
                "-workers",
                str(self._number_of_workers),
                "-typeshed",
                str(self._configuration.get_typeshed()),
            ]
        )
        search_path = self._configuration.get_search_path()
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])
        if self._log_identifier:
            flags.extend(["-log-identifier", self._log_identifier])
        if self._logger:
            flags.extend(["-logger", self._logger])
        return flags

    def _run(self, retries: int = 1) -> None:
        result = self._call_client(command=self.NAME)
        errors = self._get_errors(result)
        self._print(errors)

        if errors:
            self._exit_code = FOUND_ERRORS

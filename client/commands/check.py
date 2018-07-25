# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from .. import FOUND_ERRORS, SUCCESS
from .error_handling import ErrorHandling


class Check(ErrorHandling):
    NAME = "check"

    def __init__(self, arguments, configuration, source_directory) -> None:
        super(Check, self).__init__(arguments, configuration, source_directory)
        self._log_identifier = arguments.log_identifier
        self._number_of_workers = configuration.number_of_workers

    def _flags(self):
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        # Check the length in order to only add the flag when we have a
        # strict superset.
        if len(filter_directories) > 1 and filter_directories.issuperset(
            {self._source_directory}
        ):
            flags.extend(["-filter-directories", ",".join(list(filter_directories))])
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
        return flags

    def _run(self, retries: int = 1) -> int:
        result = self._call_client(command=self.NAME, flags=self._flags())
        errors = self._get_errors(result)
        self._print(errors)

        return FOUND_ERRORS if errors else SUCCESS

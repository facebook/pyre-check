# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import List

from .command import ExitCode
from .reporting import Reporting


class Check(Reporting):
    NAME = "check"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Check, self).__init__(arguments, configuration, analysis_directory)
        self._number_of_workers = configuration.number_of_workers

    def _flags(self) -> List[str]:
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])
        flags.extend(
            [
                "-workers",
                str(self._number_of_workers),
                "-typeshed",
                self._configuration.typeshed,
            ]
        )
        search_path = self._configuration.search_path
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])
        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])
        extensions = self._configuration.extensions
        for extension in extensions:
            flags.extend(["-extension", extension])
        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()

        result = self._call_client(command="check")
        errors = self._get_errors(result)
        self._print(errors)

        if errors:
            self._exit_code = ExitCode.FOUND_ERRORS

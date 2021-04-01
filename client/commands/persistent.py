# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import re
import select
import sys
import time
from typing import List, Optional

from .. import (
    analysis_directory,
    buck,
    command_arguments,
    configuration as configuration_module,
)
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..exceptions import EnvironmentException
from .command import Command, ExitCode, IncrementalStyle
from .start import Start


LOG: logging.Logger = logging.getLogger(__name__)


class Persistent(Command):
    NAME = "persistent"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        no_watchman: bool,
    ) -> None:
        super(Persistent, self).__init__(
            command_arguments, original_directory, configuration, analysis_directory
        )
        self._no_watchman: bool = no_watchman

    def _run(self) -> None:
        try:
            Start(
                self._command_arguments,
                self._original_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=not self._no_watchman,
                incremental_style=IncrementalStyle.FINE_GRAINED,
                configuration=self._configuration,
                analysis_directory=self._analysis_directory,
            ).run()

            self._call_client(command=self.NAME, capture_output=False).check()
        except (
            analysis_directory.NotWithinLocalConfigurationException,
            buck.BuckException,
            configuration_module.InvalidConfiguration,
            EnvironmentException,
        ) as error:
            LOG.warning(f"Running null server due to client exception: {error}")
            Persistent.run_null_server(timeout=3600 * 12)
            self._exit_code = ExitCode.SUCCESS

    def _flags(self) -> List[str]:
        flags = [
            "-log-identifier",
            f'"{self._analysis_directory.get_root()}"',
            "-expected-binary-version",
            self._configuration.get_version_hash_respecting_override() or "unversioned",
        ]
        if self._configuration.autocomplete:
            flags.append("-autocomplete")
        flags.extend(["-project-root", self._configuration.project_root])
        flags.extend(["-log-directory", self._configuration.log_directory])
        flags += self._feature_flags()
        return flags

    @classmethod
    def _initialize_response(cls, request_id: int) -> str:
        response = json.dumps(
            {"jsonrpc": "2.0", "id": request_id, "result": {"capabilities": {}}},
            sort_keys=True,
        )
        return f"Content-Length: {len(response)}\r\n\r\n{response}\r\n"

    @classmethod
    def run_null_server(cls, timeout: Optional[int] = None) -> None:
        to_read, _, _ = select.select([sys.stdin], [], [], 3.0)
        request_id = 0
        if to_read:
            standard_input = to_read[0]
            # Read content of the form Content-Length:n\r\n\r\n{jsonmessage}
            line = standard_input.readline()
            match = re.match(r"Content-Length: (?P<bytes>[0-9]+)", line)
            if match:
                length = int(match.group("bytes"))
                serialized_json = standard_input.read(length)
                try:
                    parsed = json.loads(serialized_json)
                    request_id = parsed["id"]
                # This is a catch-all to ensure that the null server always
                # gets spawned.
                except Exception:
                    pass

        sys.stdout.write(cls._initialize_response(request_id))
        sys.stdout.flush()
        start_time = int(time.time())
        while True:
            if timeout is not None and timeout <= (int(time.time()) - start_time):
                break
            time.sleep(10)

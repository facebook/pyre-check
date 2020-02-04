# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import re
import select
import sys
import time
from typing import List, Optional

from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .command import Command, IncrementalStyle
from .start import Start


class Persistent(Command):
    NAME = "persistent"

    def __init__(
        self,
        arguments: argparse.Namespace,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        super(Persistent, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._no_watchman: bool = arguments.no_watchman

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        persistent = parser.add_parser(
            cls.NAME,
            epilog="""
            Entry point for IDE integration to Pyre. Communicates with a
            Pyre server using the Language Server Protocol, accepts input from stdin and
            writing diagnostics and responses from the Pyre server to stdout.
            """,
        )
        persistent.set_defaults(command=cls, noninteractive=True)
        persistent.add_argument(
            "--no-watchman",
            action="store_true",
            help="Do not spawn a watchman client in the background.",
        )

    def _run(self) -> None:
        arguments = self._arguments
        arguments.terminal = False
        arguments.store_type_check_resolution = False
        arguments.incremental_style = IncrementalStyle.FINE_GRAINED
        Start(
            arguments,
            self._original_directory,
            self._configuration,
            self._analysis_directory,
        ).run()

        self._call_client(command=self.NAME, capture_output=False).check()

    def _flags(self) -> List[str]:
        flags = [
            "-log-identifier",
            '"{}"'.format(self._analysis_directory.get_root()),
            "-expected-binary-version",
            self._configuration.version_hash,
        ]
        if self._configuration.autocomplete:
            flags.append("-autocomplete")

        if self._log_directory:
            flags.extend(["-log-directory", self._log_directory])
        flags += self._feature_flags()
        return flags

    @classmethod
    def _initialize_response(cls, request_id: int) -> str:
        response = json.dumps(
            {"jsonrpc": "2.0", "id": request_id, "result": {"capabilities": {}}},
            sort_keys=True,
        )
        return "Content-Length: {}\r\n\r\n{}\r\n".format(len(response), response)

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

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import re
import select
import subprocess
import sys
import time
from typing import Optional

from .. import EnvironmentException, log_statistics
from .command import ClientException, Command
from .restart import Restart


class Persistent(Command):
    NAME = "persistent"

    def __init__(self, arguments, configuration, source_directory) -> None:
        super(Persistent, self).__init__(arguments, configuration, source_directory)

    def _run(self) -> None:
        arguments = self._arguments
        log_identifier = self._source_directory

        flags = [
            "-log-identifier",
            '"{}"'.format(log_identifier),
            "-version",
            str(self._configuration.get_version_hash()),
        ]
        try:
            self._call_client(
                command=self.NAME, capture_output=False, flags=flags
            ).check()
        except (ClientException, EnvironmentException, subprocess.CalledProcessError):
            arguments.terminal = False
            Restart(
                arguments, self._configuration, self._source_directory, blocking=False
            ).run()
            self._call_client(
                command=self.NAME, capture_output=False, flags=flags
            ).check()

    def on_client_exception(self) -> None:
        self._run_null_server(timeout=300)

    def _initialize_response(self, request_id: int) -> str:
        response = json.dumps(
            {"jsonrpc": "2.0", "id": request_id, "result": {"capabilities": {}}}
        )
        return "Content-Length: {}\r\n\r\n{}\r\n".format(len(response), response)

    def _run_null_server(self, timeout: Optional[int] = None) -> None:
        log_statistics(
            "perfpipe_pyre_events",
            self._arguments,
            self._configuration,
            normals={"name": "null_server_launch"},
        )
        to_read, _, _ = select.select([sys.stdin], [], [], 3.0)
        request_id = 0
        if to_read:
            standard_input = to_read[0]
            # Read content of the form Content-Length:n\r\n\r\n{jsonmessage}
            line = standard_input.readline()
            match = re.match(r"Content-Length: (?P<bytes>[0-9]+)", line)
            if match:
                length = int(match.group("bytes"))
                standard_input.readline()
                serialized_json = standard_input.read(length)
                try:
                    parsed = json.loads(serialized_json)
                    request_id = parsed["id"]
                # This is a catch-all to ensure that the null server always
                # gets spawned.
                except Exception:
                    pass

        sys.stdout.write(self._initialize_response(request_id))
        sys.stdout.flush()
        start_time = int(time.time())
        while True:
            if timeout is not None and timeout <= (int(time.time()) - start_time):
                break
            time.sleep(10)

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import re
import select
import sys
import time
from typing import List, Optional

from .command import Command
from .start import Start


class Persistent(Command):
    NAME = "persistent"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super(Persistent, self).__init__(arguments, configuration, analysis_directory)

    def _run(self) -> None:
        arguments = self._arguments
        arguments.terminal = False
        arguments.store_type_check_resolution = False
        Start(arguments, self._configuration, self._analysis_directory).run()

        self._call_client(command=self.NAME, capture_output=False).check()

    def _flags(self) -> List[str]:
        return [
            "-log-identifier",
            '"{}"'.format(self._analysis_directory.get_root()),
            "-expected-binary-version",
            self._configuration.version_hash,
        ]

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
                standard_input.readline()
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

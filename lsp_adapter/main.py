# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio
import json
import re
import sys
from asyncio.events import AbstractEventLoop
from typing import Optional, Union

from ..client.json_rpc import Response


def _null_initialize_response(request_id: Optional[Union[str, int]]) -> None:
    response = Response(id=request_id, result={"capabilities": {}})
    response.write(sys.stdout.buffer)


class AdapterProtocol(asyncio.Protocol):
    def data_received(self, data: bytes) -> None:
        body = re.sub(r"Content-Length:.+\d*\r\n\r\n", "", data.decode("utf-8"))
        json_body = json.loads(body)
        _null_initialize_response(json_body["id"])


def main() -> None:
    loop: AbstractEventLoop = asyncio.get_event_loop()
    try:
        stdin_pipe_reader = loop.connect_read_pipe(AdapterProtocol, sys.stdin)
        loop.run_until_complete(stdin_pipe_reader)
        loop.run_forever()
    finally:
        loop.close()


if __name__ == "__main__":
    main()

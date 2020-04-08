# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import asyncio
import json
import re
import sys
from asyncio.events import AbstractEventLoop
from typing import Optional, Union

from ..client.json_rpc import Response


def _should_run_null_server(null_server_flag: bool) -> bool:
    # TODO[T58989824]: We also need to check if the project can be run here.
    # Needs updating to mimic the current implementation (i.e. catch the buck errors)
    return null_server_flag


def _null_initialize_response(request_id: Optional[Union[str, int]]) -> None:
    response = Response(id=request_id, result={"capabilities": {}})
    response.write(sys.stdout.buffer)


class NullServerAdapterProtocol(asyncio.Protocol):
    def data_received(self, data: bytes) -> None:
        body = re.sub(r"Content-Length:.+\d*\r\n\r\n", "", data.decode("utf-8"))
        json_body = json.loads(body)
        _null_initialize_response(json_body["id"])


class AdapterProtocol(asyncio.Protocol):
    def data_received(self, data: bytes) -> None:
        # TODO[T58989824]: Send request to running pyre server
        pass


def main(arguments: argparse.Namespace) -> None:
    loop: AbstractEventLoop = asyncio.get_event_loop()
    try:
        if _should_run_null_server(arguments.null_server):
            stdin_pipe_reader = loop.connect_read_pipe(
                NullServerAdapterProtocol, sys.stdin
            )
        else:
            stdin_pipe_reader = loop.connect_read_pipe(AdapterProtocol, sys.stdin)
        loop.run_until_complete(stdin_pipe_reader)
        loop.run_forever()
    finally:
        loop.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="LSP adapter for Pyre.")
    parser.add_argument("--null-server", default=False, action="store_true")
    arguments: argparse.Namespace = parser.parse_args()
    main(arguments)

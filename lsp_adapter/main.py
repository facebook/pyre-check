# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import asyncio
import json
import re
import subprocess
import sys
from asyncio.events import AbstractEventLoop
from pathlib import Path
from typing import Optional, Union

from ..client.find_directories import find_local_root
from ..client.json_rpc import Response
from ..client.resources import log_directory


def _should_run_null_server(null_server_flag: bool) -> bool:
    # TODO[T58989824]: We also need to check if the project can be run here.
    # Needs updating to mimic the current implementation (i.e. catch the buck errors)
    return null_server_flag


def socket_exists(current_directory: str) -> bool:
    local_root = find_local_root(original_directory=current_directory)
    return Path.exists(
        log_directory(current_directory, local_root, "server") / "adapter.sock"
    )


def start_server(current_directory: str) -> None:
    subprocess.run(["pyre", "start"], cwd=current_directory)


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
    root = arguments.root
    loop: AbstractEventLoop = asyncio.get_event_loop()
    try:
        if not socket_exists(root):
            start_server(root)
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
    parser.add_argument("--root", type=str, required=True)
    arguments: argparse.Namespace = parser.parse_args()
    main(arguments)

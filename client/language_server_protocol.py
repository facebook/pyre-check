# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
from typing import Any, BinaryIO, Dict, Optional


JSON = Dict[str, Any]


class LanguageServerProtocolMessage(object):
    def __init__(
        self, method: str, id: Optional[str] = None, parameters: Optional[JSON] = None
    ) -> None:
        self.method = method
        self.id = id
        self.parameters = parameters

    def json(self) -> str:
        object = {"jsonrpc": "2.0", "method": self.method}  # type: Dict[str, Any]
        if self.id:
            object["id"] = self.id
        if self.parameters:
            object["params"] = self.parameters
        return json.dumps(object)


def parse_content_length(line: bytes) -> Optional[int]:
    if line.startswith(b"Content-Length:"):
        length = line.split(b"Content-Length:")[1].strip()
        try:
            return int(length)
        except ValueError:
            return None
    return None


def validate_payload(payload: JSON) -> bool:
    return payload.get("jsonrpc") == "2.0" and payload.get("method")


def read_message(file: BinaryIO) -> Optional[LanguageServerProtocolMessage]:
    try:
        line = file.readline()
        length = parse_content_length(line)
        if not length:
            return None

        # Read header lines until the empty line
        while line.strip():
            line = file.readline()

        body = file.read(length)
        payload = json.loads(body.decode("utf-8"))
        if validate_payload(payload):
            return LanguageServerProtocolMessage(
                method=payload.get("method"),
                id=payload.get("id"),
                parameters=payload.get("params"),
            )
        return None
    except (ValueError, OSError, json.decoder.JSONDecodeError):
        return None


def write_message(file: BinaryIO, message: LanguageServerProtocolMessage) -> bool:
    try:
        payload = message.json()
        length = len(payload.encode("utf-8"))

        response = ("Content-Length: {}\r\n\r\n{}").format(length, payload)
        file.write(response.encode("utf-8"))
        file.flush()
        return True
    except (ValueError, OSError):
        return False


def perform_handshake(
    input_file: BinaryIO, output_file: BinaryIO, client_version: str
) -> None:
    server_handshake = read_message(input_file)
    if (
        server_handshake
        and server_handshake.method == "handshake/server"
        and server_handshake.parameters
    ):
        server_version = server_handshake.parameters.get("version")
        if server_version != client_version:
            raise ValueError(
                "Version mismatch. Server has version `{}`, "
                "while client has version `{}`.".format(server_version, client_version)
            )
        client_handshake = LanguageServerProtocolMessage(method="handshake/client")
        write_message(output_file, client_handshake)
    else:
        raise ValueError("Handshake from server was malformed.")

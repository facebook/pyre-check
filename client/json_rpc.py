# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import json
from abc import abstractmethod
from enum import Enum
from json.decoder import JSONDecodeError
from typing import Any, BinaryIO, Dict, Optional, Union


JSON = Dict[str, Any]


class LanguageServerMessageType(Enum):
    """Message type for an LSP warning message."""

    WARNING = 2
    INFORMATION = 3


class JSONRPCException(Exception):
    pass


class JSONRPC(object):
    @abstractmethod
    def json(self) -> str:
        pass

    def format(self) -> bytes:
        payload = self.json()
        length = len(payload.encode("utf-8"))

        response = ("Content-Length: {}\r\n\r\n{}").format(length, payload)
        return response.encode("utf-8")

    def write(self, file: BinaryIO) -> bool:
        try:
            file.write(self.format())
            file.flush()
            return True
        except (ValueError, OSError):
            return False

    @classmethod
    def from_json(cls, json_body: JSON) -> "JSONRPC":
        if "params" in json_body.keys():
            return Request.from_json(json_body)
        else:
            return Response.from_json(json_body)


class Request(JSONRPC):
    def __init__(
        self, method: str, id: Optional[str] = None, parameters: Optional[JSON] = None
    ) -> None:
        self.method = method
        self.id = id
        self.parameters = parameters

    def json(self) -> str:
        json_object: Dict[str, Any] = {"jsonrpc": "2.0", "method": self.method}
        if self.id is not None:
            json_object["id"] = self.id
        if self.parameters is not None:
            json_object["params"] = self.parameters
        return json.dumps(json_object)

    @classmethod
    def from_json(cls, json: JSON) -> "Request":
        request_keys = json.keys()
        method = json["method"]
        id = json["id"] if "id" in request_keys else None
        parameters = json["params"] if "params" in request_keys else None
        return Request(method=method, id=id, parameters=parameters)

    @staticmethod
    def validate_payload(payload: JSON) -> bool:
        return payload.get("jsonrpc") == "2.0" and payload.get("method") is not None


class Response(JSONRPC):
    def __init__(
        self,
        result: Optional[JSON] = None,
        id: Optional[Union[str, int]] = None,
        error: Optional[JSON] = None,
    ) -> None:
        self.result = result
        self.id = id
        self.error = error

    @staticmethod
    def validate_payload(payload: JSON) -> bool:
        return (
            payload.get("jsonrpc") == "2.0"
            and "result" in payload
            and "error" in payload
        )

    def json(self) -> str:
        return json.dumps({"jsonrpc": "2.0", "id": self.id, "result": self.result})

    @classmethod
    def from_json(cls, json: JSON) -> "Response":
        response_keys = json.keys()
        id = json["id"] if "id" in response_keys else None
        result = json["result"] if "result" in response_keys else None
        error = json["error"] if "error" in response_keys else None
        return Response(result=result, id=id, error=error)


def parse_content_length(line: bytes) -> Optional[int]:
    if line.startswith(b"Content-Length:"):
        length = line.split(b"Content-Length:")[1].strip()
        try:
            return int(length)
        except ValueError:
            return None
    return None


def _read_payload(file: BinaryIO) -> Optional[JSON]:
    try:
        line = file.readline()
        length = parse_content_length(line)
        if not length:
            return None

        # Read header lines until the empty line
        while line.strip():
            line = file.readline()

        body = file.read(length)
        return json.loads(body.decode("utf-8"))
    except (ValueError, OSError, JSONDecodeError):
        return None


def read_request(file: BinaryIO) -> Optional[Request]:
    payload = _read_payload(file)
    if payload and Request.validate_payload(payload):
        return Request(
            method=payload.get("method"),
            id=payload.get("id"),
            parameters=payload.get("params"),
        )
    return None


def read_response(file: BinaryIO) -> Response:
    payload = _read_payload(file)
    if not payload:
        raise JSONRPCException("Received empty response.")
    if not Response.validate_payload(payload):
        raise JSONRPCException(f"Unable to validate response: {str(payload)}")
    return Response(
        result=payload.get("result"), error=payload.get("error"), id=payload.get("id")
    )


def perform_handshake(
    input_file: BinaryIO, output_file: BinaryIO, client_version: str
) -> None:
    server_handshake = read_request(input_file)
    if server_handshake and server_handshake.method == "handshake/server":
        server_handshake_parameters = server_handshake.parameters
        if server_handshake_parameters:
            server_version = server_handshake_parameters.get("version")
            if server_version != client_version:
                raise ValueError(
                    "Version mismatch. Server has version `{}`, "
                    "while client has version `{}`.".format(
                        server_version, client_version
                    )
                )
            client_handshake = Request(
                method="handshake/client", parameters={"send_confirmation": True}
            )
            client_handshake.write(output_file)
            request = read_request(input_file)
            if not (request and request.method == "handshake/socket_added"):
                raise ValueError("Handshake was not successful.")
        else:
            raise ValueError("Handshake parameters from server not found.")
    else:
        raise ValueError("Handshake from server was malformed.")

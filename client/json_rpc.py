# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import abc
import dataclasses
import json
from enum import Enum
from json.decoder import JSONDecodeError
from typing import Any, Dict, Mapping, Optional, Sequence, Union


JSON = Dict[str, Any]
JSONRPC_VERSION = "2.0"


class LanguageServerMessageType(Enum):
    """Message type for an LSP warning message."""

    WARNING = 2
    INFORMATION = 3


class JSONRPCException(Exception, metaclass=abc.ABCMeta):
    """
    Base class of all jsonrpc related errors.
    """

    @abc.abstractmethod
    def error_code(self) -> int:
        raise NotImplementedError()


class ParseError(JSONRPCException):
    """
    An error occurred on the server while parsing the JSON text.
    """

    def error_code(self) -> int:
        return -32700


class InvalidRequestError(JSONRPCException):
    """
    The JSON received is not a valid Request object.
    Internally we also raise it when the JSON sent is not a valid Response object.
    """

    def error_code(self) -> int:
        return -32600


class MissingMethodFieldInRequestError(InvalidRequestError):
    """
    Special case of InvalidRequestError, used when the "method" field
    is missing in a request.

    This error type is needed because VSCode sends us many empty requests
    that have no method and we need a specific exception that makes it easy
    to ignore these messages.
    """

    pass


class MethodNotFoundError(JSONRPCException):
    """
    The method does not exist / is not available.
    """

    def error_code(self) -> int:
        return -32601


class InvalidParameterError(JSONRPCException):
    """
    Invalid method parameter(s).
    """

    def error_code(self) -> int:
        return -32602


class InternalError(JSONRPCException):
    """
    Internal JSON-RPC error.
    """

    def error_code(self) -> int:
        return -32603


class JSONRPC(abc.ABC):
    @abc.abstractmethod
    def json(self) -> JSON:
        raise NotImplementedError()

    def serialize(self) -> str:
        return json.dumps(self.json())


def _verify_json_rpc_version(json: JSON) -> None:
    json_rpc_version = json.get("jsonrpc")
    if json_rpc_version is None:
        raise InvalidRequestError(f"Required field `jsonrpc` is missing: {json}")
    if json_rpc_version != JSONRPC_VERSION:
        raise InvalidRequestError(
            f"`jsonrpc` is expected to be '2.0' but got '{json_rpc_version}'"
        )


def _parse_json_rpc_id(json: JSON) -> Union[int, str, None]:
    id = json.get("id")
    if id is not None and not isinstance(id, int) and not isinstance(id, str):
        raise InvalidRequestError(
            f"Request ID must be either an integer or string but got {id}"
        )
    return id


def _parse_json_rpc_activity_key(json: JSON) -> Optional[JSON]:
    activity_key = json.get("activityKey")
    if activity_key is None:
        return None
    elif isinstance(activity_key, dict):
        return activity_key
    else:
        raise InvalidParameterError(
            f"Cannot parse request activityKey JSON: {activity_key}"
        )


@dataclasses.dataclass(frozen=True)
class ByPositionParameters:
    values: Sequence[object] = dataclasses.field(default_factory=list)


@dataclasses.dataclass(frozen=True)
class ByNameParameters:
    values: Mapping[str, object] = dataclasses.field(default_factory=dict)


Parameters = Union[ByPositionParameters, ByNameParameters]


@dataclasses.dataclass(frozen=True)
class Request(JSONRPC):
    method: str
    id: Union[int, str, None] = None
    activity_key: Optional[JSON] = None
    parameters: Optional[Parameters] = None

    def json(self) -> JSON:
        parameters = self.parameters
        return {
            "jsonrpc": JSONRPC_VERSION,
            "method": self.method,
            **({"id": self.id} if self.id is not None else {}),
            **(
                {"activityKey": self.activity_key}
                if self.activity_key is not None
                else {}
            ),
            **({"params": parameters.values} if parameters is not None else {}),
        }

    def extract_parameters(self) -> Parameters:
        parameters = self.parameters
        if parameters is None:
            raise InvalidRequestError(
                f"No parameters to extract for JSON-RPC {self.method} method"
            )
        return parameters

    @staticmethod
    def from_json(request_json: JSON) -> "Request":
        """
        Parse a given JSON into a JSON-RPC request.
        Raises `InvalidRequestError` and `InvalidParameterError` if the JSON
        body is malformed.
        """
        _verify_json_rpc_version(request_json)

        method = request_json.get("method")
        if method is None:
            raise MissingMethodFieldInRequestError(
                f"Required field `method` is missing: {request_json}"
            )
        if not isinstance(method, str):
            raise InvalidRequestError(
                f"`method` is expected to be a string but got {method}"
            )

        raw_parameters = request_json.get("params")
        if raw_parameters is None:
            parameters = None
        elif isinstance(raw_parameters, list):
            parameters = ByPositionParameters(raw_parameters)
        elif isinstance(raw_parameters, dict):
            parameters = ByNameParameters(raw_parameters)
        else:
            raise InvalidParameterError(
                f"Cannot parse request parameter JSON: {raw_parameters}"
            )

        id = _parse_json_rpc_id(request_json)
        activity_key = _parse_json_rpc_activity_key(request_json)
        return Request(
            method=method, id=id, activity_key=activity_key, parameters=parameters
        )

    @staticmethod
    def from_string(request_string: str) -> "Request":
        """
        Parse a given string into a JSON-RPC request.
        - Raises `ParseError` if json parsing fails.
        - Raises `InvalidRequestError` and `InvalidParameterError` if the
          JSON body is malformed (in any way other than a missing `method`)
        """
        try:
            request_json = json.loads(request_string)
            return Request.from_json(request_json)
        except JSONDecodeError as error:
            message = f"Cannot parse string into JSON: {error}"
            raise ParseError(message) from error


@dataclasses.dataclass(frozen=True)
class Response(JSONRPC):
    id: Union[int, str, None]

    @staticmethod
    def from_json(response_json: JSON) -> "Response":
        """
        Parse a given JSON into a JSON-RPC response.
        Raises `InvalidRequestError` if the JSON body is malformed.
        """
        if "result" in response_json:
            return SuccessResponse.from_json(response_json)
        elif "error" in response_json:
            return ErrorResponse.from_json(response_json)
        else:
            raise InvalidRequestError(
                "Either `result` or `error` must be presented in JSON-RPC "
                + f"responses. Got {response_json}."
            )

    @staticmethod
    def from_string(response_string: str) -> "Response":
        """
        Parse a given string into a JSON-RPC response.
        Raises `ParseError` if the parsing fails. Raises `InvalidRequestError`
        if the JSON body is malformed.
        """
        try:
            response_json = json.loads(response_string)
            return Response.from_json(response_json)
        except JSONDecodeError as error:
            message = f"Cannot parse string into JSON: {error}"
            raise ParseError(message) from error


@dataclasses.dataclass(frozen=True)
class SuccessResponse(Response):
    result: object
    activity_key: Optional[JSON] = None

    def json(self) -> JSON:
        return {
            "jsonrpc": JSONRPC_VERSION,
            **({"id": self.id} if self.id is not None else {}),
            **(
                {"activityKey": self.activity_key}
                if self.activity_key is not None
                else {}
            ),
            "result": self.result,
        }

    @staticmethod
    def from_json(response_json: JSON) -> "SuccessResponse":
        """
        Parse a given JSON into a JSON-RPC success response.
        Raises `InvalidRequestError` if the JSON body is malformed.
        """
        _verify_json_rpc_version(response_json)

        result = response_json.get("result")
        if result is None:
            raise InvalidRequestError(
                f"Required field `result` is missing: {response_json}"
            )

        # FIXME: The `id` field is required for the respnose, but we can't
        # enforce it right now since the Pyre server may emit id-less responses
        # and that has to be fixed first.
        id = _parse_json_rpc_id(response_json)
        activity_key = _parse_json_rpc_activity_key(response_json)
        return SuccessResponse(id=id, activity_key=activity_key, result=result)


@dataclasses.dataclass(frozen=True)
class ErrorResponse(Response):
    code: int
    message: str = ""
    data: Optional[object] = None
    activity_key: Optional[JSON] = None

    def json(self) -> JSON:
        return {
            "jsonrpc": JSONRPC_VERSION,
            **({"id": self.id} if self.id is not None else {}),
            **(
                {"activityKey": self.activity_key}
                if self.activity_key is not None
                else {}
            ),
            "error": {
                "code": self.code,
                "message": self.message,
                **({"data": self.data} if self.data is not None else {}),
            },
        }

    @staticmethod
    def from_json(response_json: JSON) -> "ErrorResponse":
        """
        Parse a given JSON into a JSON-RPC error response.
        Raises `InvalidRequestError` if the JSON body is malformed.
        """
        _verify_json_rpc_version(response_json)

        error = response_json.get("error")
        if error is None:
            raise InvalidRequestError(
                f"Required field `error` is missing: {response_json}"
            )
        if not isinstance(error, dict):
            raise InvalidRequestError(f"`error` must be a dict but got {error}")

        code = error.get("code")
        if code is None:
            raise InvalidRequestError(
                f"Required field `error.code` is missing: {response_json}"
            )
        if not isinstance(code, int):
            raise InvalidRequestError(
                f"`error.code` is expected to be an int but got {code}"
            )

        message = error.get("message", "")
        if not isinstance(message, str):
            raise InvalidRequestError(
                f"`error.message` is expected to be a string but got {message}"
            )

        data = error.get("data")
        # FIXME: The `id` field is required for the respnose, but we can't
        # enforce it right now since the Pyre server may emit id-less responses
        # and that has to be fixed first.
        id = _parse_json_rpc_id(response_json)
        activity_key = _parse_json_rpc_activity_key(response_json)
        return ErrorResponse(
            id=id, activity_key=activity_key, code=code, message=message, data=data
        )

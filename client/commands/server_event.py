# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides Python event data structures to represent
interactions with the Pyre daemon, as well as an async utility
cann connect to a socket and wait (used when initializing new
daemons).
"""

import dataclasses
import enum
import json
from pathlib import Path
from typing import IO, List, Optional, Union

from ..language_server import connections
from .commands import ExitCode


@dataclasses.dataclass
class SocketCreated:
    socket_path: Path


@dataclasses.dataclass
class ServerInitialized:
    pass


class ErrorKind(enum.Enum):
    WATCHMAN = "Watchman"
    BUCK_INTERNAL = "BuckInternal"
    BUCK_USER = "BuckUser"
    PYRE = "Pyre"
    UNKNOWN = "Unknown"

    def __str__(self) -> str:
        return self.value

    @staticmethod
    def from_string(input_string: str) -> "ErrorKind":
        for item in ErrorKind:
            if input_string == str(item):
                return item
        return ErrorKind.UNKNOWN

    def to_exit_code(kind) -> ExitCode:
        if kind == ErrorKind.WATCHMAN:
            return ExitCode.WATCHMAN_ERROR
        elif kind == ErrorKind.BUCK_INTERNAL:
            return ExitCode.BUCK_INTERNAL_ERROR
        elif kind == ErrorKind.BUCK_USER:
            return ExitCode.BUCK_USER_ERROR
        return ExitCode.FAILURE


@dataclasses.dataclass
class ServerException:
    message: str
    kind: ErrorKind = ErrorKind.UNKNOWN


Event = Union[SocketCreated, ServerInitialized, ServerException]


def create_from_string(input_string: str) -> Optional[Event]:
    try:
        input_json: List[str] = json.loads(input_string)
        if len(input_json) < 1:
            return None

        input_kind = input_json[0]
        if input_kind == "SocketCreated":
            if len(input_json) < 2:
                return None
            else:
                return SocketCreated(socket_path=Path(input_json[1]))
        elif input_kind == "ServerInitialized":
            return ServerInitialized()
        elif input_kind == "Exception":
            if len(input_json) < 2:
                return None
            if not isinstance(input_json[1], str):
                return None
            if (
                len(input_json) >= 3
                and isinstance(input_json[2], list)
                and len(input_json[2]) > 0
                and isinstance(input_json[2][0], str)
            ):
                return ServerException(
                    message=input_json[1], kind=ErrorKind.from_string(input_json[2][0])
                )
            return ServerException(message=input_json[1], kind=ErrorKind.UNKNOWN)
        else:
            return None
    except json.JSONDecodeError:
        return None


class EventParsingException(Exception):
    pass


class ServerStartException(Exception):
    kind: ErrorKind

    def __init__(self, exception_event: ServerException) -> None:
        super().__init__(exception_event.message)
        self.kind = exception_event.kind


def _parse_server_event(event_string: str) -> Event:
    event = create_from_string(event_string)
    if event is None:
        raise EventParsingException(
            f"Unrecognized status update from server: {event_string}"
        )
    elif isinstance(event, ServerException):
        raise ServerStartException(event)
    return event


class Waiter:
    wait_on_initialization: bool

    def __init__(self, wait_on_initialization: bool) -> None:
        self.wait_on_initialization = wait_on_initialization

    def wait_on(self, event_stream: IO[str]) -> None:
        """
        Read from the given input channel, expecting server events there.
        If `self.wait_on_initialization` is false, block until server socket
        creation and returns.
        Otherwise, block until server initialization has finished and returns.
        If data obtained from the input channel does not conform to the server
        event format, raise `EventParsingException`. If an error event is received,
        raise `ServerStartException`.
        """
        # The first event is expected to be socket creation
        initial_event = _parse_server_event(event_stream.readline().strip())
        if isinstance(initial_event, SocketCreated):
            if not self.wait_on_initialization:
                return

            # The second event is expected to be server initialization
            second_event = _parse_server_event(event_stream.readline().strip())
            if isinstance(second_event, ServerInitialized):
                return

            raise EventParsingException(
                f"Unexpected second server status update: {second_event}"
            )

        raise EventParsingException(
            f"Unexpected initial server status update: {initial_event}"
        )

    # This method does the same thing as `wait_on` except it operates on asyncio
    # streams rather than synchronoized streams.
    # NOTE: Any changes inside `wait_on` need to be applied here as well.
    async def async_wait_on(self, event_stream: connections.AsyncTextReader) -> None:
        """
        This method is the same as `wait_on`, except it operates on async input
        channels instead of synchronous ones.
        """
        initial_event = _parse_server_event((await event_stream.readline()).strip())
        if isinstance(initial_event, SocketCreated):
            if not self.wait_on_initialization:
                return

            second_event = _parse_server_event((await event_stream.readline()).strip())
            if isinstance(second_event, ServerInitialized):
                return

            raise EventParsingException(
                f"Unexpected second server status update: {second_event}"
            )

        raise EventParsingException(
            f"Unexpected initial server status update: {initial_event}"
        )

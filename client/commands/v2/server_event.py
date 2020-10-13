# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
from pathlib import Path
from typing import List, Optional, Union


@dataclasses.dataclass
class SocketCreated:
    socket_path: Path


@dataclasses.dataclass
class ServerInitialized:
    pass


@dataclasses.dataclass
class ServerException:
    message: str


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
            else:
                return ServerException(message=input_json[1])
        else:
            return None
    except json.JSONDecodeError:
        return None

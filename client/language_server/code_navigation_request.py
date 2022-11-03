# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module contains the definition of code navigation requests and an API to convert a given LSP request
to a corresponding code navigation request. Also contains an API that sends a given request to the code navigation
server and gets a response.
"""
import dataclasses
import json
from pathlib import Path
from typing import List, Optional, Union

from . import daemon_connection, protocol as lsp


@dataclasses.dataclass
class HoverRequest:
    path: Path
    overlay_id: Optional[str]
    position: lsp.PyrePosition

    def to_json(self) -> List[object]:
        return [
            "Hover",
            {
                "module": ["OfPath", f"{self.path}"],
                "overlay_id": self.overlay_id,
                "position": {
                    "line": self.position.line,
                    "column": self.position.character,
                },
            },
        ]


async def async_handle_hover_request(
    socket_path: Path,
    hover_request: HoverRequest,
) -> Union[lsp.LspHoverResponse, daemon_connection.DaemonConnectionFailure]:
    raw_request = json.dumps(["Query", hover_request.to_json()])
    response = await daemon_connection.attempt_send_async_raw_request(
        socket_path, raw_request
    )
    if isinstance(response, daemon_connection.DaemonConnectionFailure):
        return response

    return lsp.LspHoverResponse(contents=response)

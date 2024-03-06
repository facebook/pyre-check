# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module contains logic for querying a socket connection to a
Pyre code-navigation daemon. See ./daemon_launcher.py for how to
start and stop such daemons.
"""

from __future__ import annotations

from pathlib import Path
from typing import Optional, Set

from ..client.language_server import code_navigation_request, daemon_connection

from . import daemon_launcher


class CodeNavConnection:
    def __init__(
        self,
        server_info: daemon_launcher.StartedServerInfo,
    ) -> None:
        self.server_info = server_info
        # All open files must be resolved to full paths before getting added here.
        self.open_files: Set[Path] = set()

    async def superclasses(
        self, module_name: str, class_name: str
    ) -> (
        code_navigation_request.ErrorResponse
        | code_navigation_request.SuperclassesResponse
    ):
        class_expression = code_navigation_request.ClassExpression(
            module=module_name,
            qualified_name=class_name,
        )
        superclasses_request = code_navigation_request.SuperclassesRequest(
            class_=class_expression,
        )
        return await code_navigation_request.async_handle_superclasses(
            self.server_info.socket_path, superclasses_request
        )

    async def open_file(
        self, path: Path
    ) -> Optional[daemon_connection.DaemonConnectionFailure]:
        path = path.resolve()
        if path in self.open_files:
            return None
        self.open_files.add(path)
        request = code_navigation_request.FileOpened(
            path=path, content=None, client_id=self.server_info.client_id
        )
        response = await code_navigation_request.async_handle_file_opened(
            self.server_info.socket_path, request
        )
        if isinstance(response, daemon_connection.DaemonConnectionFailure):
            return response
        return None

    async def close_file(
        self, path: Path
    ) -> Optional[daemon_connection.DaemonConnectionFailure]:
        path = path.resolve()
        self.open_files.remove(path)
        request = code_navigation_request.FileClosed(
            path=path, client_id=self.server_info.client_id
        )
        response = await code_navigation_request.async_handle_file_closed(
            self.server_info.socket_path, request
        )
        if isinstance(response, daemon_connection.DaemonConnectionFailure):
            return response
        return None

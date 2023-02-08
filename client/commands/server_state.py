# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module stores all server state related fields as a single class, so all the mutable state
can be accessed in one place. Note that this state is mutable and a singleton, so users of this module
should be aware a change to this state could affect other modules that interact with this state.
"""


import dataclasses
import enum
from pathlib import Path
from typing import Dict, List, Optional

from typing_extensions import Final

from .. import timer

from ..language_server import protocol as lsp
from . import pyre_server_options


class ServerStatus(enum.Enum):
    READY = "READY"
    DISCONNECTED = "DISCONNECTED"
    NOT_CONNECTED = "NOT_CONNECTED"
    SUSPENDED = "SUSPENDED"
    BUCK_BUILDING = "BUCK_BUILDING"
    INCREMENTAL_CHECK = "INCREMENTAL_CHECK"
    STARTING = "STARTING"


@dataclasses.dataclass(frozen=True)
class OpenedDocumentState:
    code: str
    is_dirty: bool = False
    pyre_code_updated: bool = False


@dataclasses.dataclass
class DaemonStatus:
    _status: ServerStatus = ServerStatus.NOT_CONNECTED
    _not_ready_timer: Optional[timer.Timer] = None

    def set(self, new_status: ServerStatus) -> None:
        if new_status == ServerStatus.READY:
            self._not_ready_timer = None
        elif self._not_ready_timer is None:
            self._not_ready_timer = timer.Timer()
        self._status = new_status

    def get(self) -> ServerStatus:
        return self._status

    def milliseconds_not_ready(self) -> int:
        if self._not_ready_timer is None:
            return 0
        return int(self._not_ready_timer.stop_in_millisecond())


@dataclasses.dataclass
class ServerState:
    # State that can only change on config reload
    server_options: pyre_server_options.PyreServerOptions

    # Immutable States
    client_capabilities: lsp.ClientCapabilities = lsp.ClientCapabilities()

    # Mutable States
    consecutive_start_failure: int = 0
    is_user_notified_on_buck_failure: bool = False
    opened_documents: Dict[Path, OpenedDocumentState] = dataclasses.field(
        default_factory=dict
    )
    diagnostics: Dict[Path, List[lsp.Diagnostic]] = dataclasses.field(
        default_factory=dict
    )

    # The daemon status is not reassignable, but has internal mutable state
    daemon_status: Final[DaemonStatus] = dataclasses.field(default_factory=DaemonStatus)

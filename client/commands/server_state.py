# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module stores all server state related fields as a single class, so all the mutable state
can be accessed in one place. Note that this state is mutable and a singleton, so users of this module
should be aware a change to this state could affect other modules that interact with this state.
"""

from __future__ import annotations

import dataclasses
import enum
from pathlib import Path
from typing import Dict, List, Optional

from typing_extensions import Final

from .. import timer

from ..language_server import protocol as lsp
from . import pyre_server_options


class ConnectionStatus(enum.Enum):
    READY = "READY"
    DISCONNECTED = "DISCONNECTED"
    NOT_CONNECTED = "NOT_CONNECTED"
    SUSPENDED = "SUSPENDED"
    BUCK_BUILDING = "BUCK_BUILDING"
    INCREMENTAL_CHECK = "INCREMENTAL_CHECK"
    STARTING = "STARTING"


@dataclasses.dataclass(frozen=True)
class DaemonStatus:
    connection_status: ConnectionStatus
    milliseconds_since_ready: float

    def as_telemetry_dict(self) -> Dict[str, float | str]:
        return {
            "server_status_before": self.connection_status.value,
            "time_since_last_ready_ms": self.milliseconds_since_ready,
        }


@dataclasses.dataclass(frozen=True)
class OpenedDocumentState:
    code: str
    is_dirty: bool = False
    pyre_code_updated: bool = False


@dataclasses.dataclass
class DaemonStatusTracker:
    _connection_status: ConnectionStatus = ConnectionStatus.NOT_CONNECTED
    _not_ready_timer: Optional[timer.Timer] = None

    def set_status(self, new_status: ConnectionStatus) -> None:
        if new_status == ConnectionStatus.READY:
            self._not_ready_timer = None
        elif self._not_ready_timer is None:
            self._not_ready_timer = timer.Timer()
        self._connection_status = new_status

    def get_status(self) -> DaemonStatus:
        return DaemonStatus(
            connection_status=self._connection_status,
            milliseconds_since_ready=(
                0
                if self._not_ready_timer is None
                else self._not_ready_timer.stop_in_millisecond()
            ),
        )


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
    status_tracker: Final[DaemonStatusTracker] = dataclasses.field(
        default_factory=DaemonStatusTracker
    )

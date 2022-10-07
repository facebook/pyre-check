# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import dataclasses
import enum
from pathlib import Path
from typing import Dict, List, Set

from .. import timer

from . import language_server_protocol as lsp, pyre_server_options


class ServerStatus(enum.Enum):
    READY = "READY"
    DISCONNECTED = "DISCONNECTED"
    NOT_CONNECTED = "NOT_CONNECTED"
    SUSPENDED = "SUSPENDED"
    BUCK_BUILDING = "BUCK_BUILDING"
    INCREMENTAL_CHECK = "INCREMENTAL_CHECK"


@dataclasses.dataclass
class ServerState:
    # State that can only change on config reload
    server_options: pyre_server_options.PyreServerOptions

    # Immutable States
    client_capabilities: lsp.ClientCapabilities = lsp.ClientCapabilities()

    # Mutable States
    consecutive_start_failure: int = 0
    is_user_notified_on_buck_failure: bool = False
    opened_documents: Set[Path] = dataclasses.field(default_factory=set)
    diagnostics: Dict[Path, List[lsp.Diagnostic]] = dataclasses.field(
        default_factory=dict
    )
    last_diagnostic_update_timer: timer.Timer = dataclasses.field(
        default_factory=timer.Timer
    )
    server_last_status: ServerStatus = ServerStatus.NOT_CONNECTED

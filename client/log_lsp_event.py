# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides shared logic for logging events - mainly
error information - in Pyre language servers (whenever telemetry is
enabled, which it is not by default in open-source Pyre).

Be default these logs go to the table pyre_lsp_events.

Note that this Pyre-side logging is distinct from editor-handled language
server telemetry, which (when enabled) is accomplished by sending special
json messages to the editor, which is responsible for loggign them.
"""

import enum
from typing import Dict, Optional

from . import backend_arguments, remote_logger, version


class LSPEvent(enum.Enum):
    INITIALIZED = "initialized"
    NOT_INITIALIZED = "not initialized"
    CONNECTED = "connected"
    NOT_CONNECTED = "not connected"
    NOT_CONFIGURED = "not configured"
    DISCONNECTED = "disconnected"
    SUSPENDED = "suspended"
    STOPPED = "stopped"
    COVERED = "covered"


def log(
    remote_logging: Optional[backend_arguments.RemoteLogging],
    event: LSPEvent,
    integers: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, Optional[str]]] = None,
) -> None:
    if remote_logging is not None:
        logger = remote_logging.logger
        if logger is not None:
            log_identifier = remote_logging.identifier
            remote_logger.log(
                category=remote_logger.LoggerCategory.LSP_EVENTS,
                logger=logger,
                integers=integers,
                normals={
                    **(normals or {}),
                    "event": event.value,
                    "pyre client version": version.__version__,
                    **(
                        {"identifier": log_identifier}
                        if log_identifier is not None
                        else {}
                    ),
                },
            )

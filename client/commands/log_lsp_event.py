# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import enum
from typing import Dict, Optional

from .. import statistics_logger, version

from . import backend_arguments


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


def _log_lsp_event(
    remote_logging: Optional[backend_arguments.RemoteLogging],
    event: LSPEvent,
    integers: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, Optional[str]]] = None,
) -> None:
    if remote_logging is not None:
        logger = remote_logging.logger
        if logger is not None:
            log_identifier = remote_logging.identifier
            statistics_logger.log(
                category=statistics_logger.LoggerCategory.LSP_EVENTS,
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

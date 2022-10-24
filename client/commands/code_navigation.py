# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This command contains the main logic for the client connecting language servers
and Pyre's code navigation server. It mainly ferries LSP requests back and forth between
editors and the backend, and handles the initialization protocol and searches for appropriate
configurations.
"""

from __future__ import annotations

import asyncio
import logging
import traceback

from typing import Optional

from .. import timer
from . import backend_arguments, log_lsp_event

LOG: logging.Logger = logging.getLogger(__name__)


async def async_run_code_navigation_client(
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    return 0


def run(
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    command_timer = timer.Timer()
    error_message: Optional[str] = None
    try:
        return asyncio.run(
            async_run_code_navigation_client(
                remote_logging,
            )
        )
    except Exception:
        error_message = traceback.format_exc()
        LOG.exception("Uncaught error in code_navigation.run")
        return 1
    finally:
        log_lsp_event._log_lsp_event(
            remote_logging,
            log_lsp_event.LSPEvent.STOPPED,
            integers={"duration": int(command_timer.stop_in_millisecond())},
            normals={
                **({"exception": error_message} if error_message is not None else {})
            },
        )

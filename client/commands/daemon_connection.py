# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import logging
from pathlib import Path

from .. import log
from . import connections


LOG: logging.Logger = logging.getLogger(__name__)


def send_raw_request(socket_path: Path, raw_request: str) -> str:
    with connections.connect(socket_path) as (
        input_channel,
        output_channel,
    ):
        LOG.debug(f"Sending `{log.truncate(raw_request, 400)}`")
        output_channel.write(f"{raw_request}\n")
        raw_response = input_channel.readline().strip()
        LOG.debug(f"Received `{log.truncate(raw_response, 400)}`")
        return raw_response

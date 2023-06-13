# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Placeholder code to bootstrap a pyre report-any-expressions command.

This command is a work in progress, not yet ready for use
"""

from __future__ import annotations

import logging
from pathlib import Path

from typing import List, Optional

from .. import frontend_configuration
from . import commands

LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: frontend_configuration.Base,
    paths: Optional[List[Path]],
) -> int:
    LOG.error("The `pyre report-any-expressions` command is not yet implemented.")
    return commands.ExitCode.FAILURE

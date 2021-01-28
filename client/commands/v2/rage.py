# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
from pathlib import Path
from typing import Optional

from ... import commands, configuration as configuration_module

LOG: logging.Logger = logging.getLogger(__name__)


def run(
    configuration: configuration_module.Configuration, output_path: Optional[Path]
) -> commands.ExitCode:
    LOG.warning("Not implemented yet")
    return commands.ExitCode.SUCCESS

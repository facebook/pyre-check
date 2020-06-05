# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
from pathlib import Path
from typing import List, Optional

from . import filesystem


LOG: logging.Logger = logging.getLogger(__name__)
RECENTLY_USED_LOCAL_CONFIGURATIONS_FILE = "recently-used-local-configurations.json"
RECENTLY_USED_LOCAL_CONFIGURATIONS_LOCK = "recently-used-local-configurations.lock"
MAXIMUM_RECENT_ITEMS = 10


def _add_recently_used_configuration(
    local_configuration: str, existing_configurations: List[str]
) -> List[str]:
    updated_configurations = [
        local_configuration,
        *(
            configuration
            for configuration in existing_configurations
            if configuration != local_configuration
        ),
    ]
    return updated_configurations[:MAXIMUM_RECENT_ITEMS]


def _load_recently_used_configurations(dot_pyre_directory: Path) -> List[str]:
    configurations = []
    recently_used_configurations_path = (
        dot_pyre_directory / RECENTLY_USED_LOCAL_CONFIGURATIONS_FILE
    )
    try:
        configurations = json.loads(recently_used_configurations_path.read_text())
    except FileNotFoundError:
        LOG.debug(f"No existing file `{str(recently_used_configurations_path)}`.")
    except json.JSONDecodeError:
        LOG.debug(
            "Error when loading json from "
            f"`{str(recently_used_configurations_path)}`"
        )
    return configurations


def log_as_recently_used(
    local_configuration: Optional[str], dot_pyre_directory: Path
) -> None:
    if not local_configuration:
        return

    lock_path = dot_pyre_directory / RECENTLY_USED_LOCAL_CONFIGURATIONS_LOCK
    try:
        with filesystem.acquire_lock(str(lock_path), blocking=False):
            recently_used_configurations_path = (
                dot_pyre_directory / RECENTLY_USED_LOCAL_CONFIGURATIONS_FILE
            )
            existing_configurations = _load_recently_used_configurations(
                dot_pyre_directory
            )
            new_configurations = _add_recently_used_configuration(
                local_configuration, existing_configurations
            )
            recently_used_configurations_path.write_text(json.dumps(new_configurations))
    except OSError:
        LOG.debug(
            f"Failed to acquire lock `{str(lock_path)}`. "
            "Not logging in recently-used configurations cache."
        )

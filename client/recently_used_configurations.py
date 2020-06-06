# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import sys
from pathlib import Path
from typing import List, Optional

from . import filesystem, terminal


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


def delete_cache(dot_pyre_directory: Path) -> None:
    try:
        os.remove(str(dot_pyre_directory / RECENTLY_USED_LOCAL_CONFIGURATIONS_LOCK))
        os.remove(str(dot_pyre_directory / RECENTLY_USED_LOCAL_CONFIGURATIONS_FILE))
    except OSError as error:
        LOG.debug(
            "Error while trying to delete recently-used configurations files: "
            f"{error}."
        )


def get_recently_used_configurations(dot_pyre_directory: Path) -> List[str]:
    lock_path = dot_pyre_directory / RECENTLY_USED_LOCAL_CONFIGURATIONS_LOCK
    try:
        with filesystem.acquire_lock(str(lock_path), blocking=False):
            return _load_recently_used_configurations(dot_pyre_directory)
    except OSError:
        LOG.debug(
            f"Failed to acquire lock `{str(lock_path)}`. "
            "Returning empty list of recently-used configurations."
        )
        return []


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


def prompt_user_for_local_root(local_roots: List[str]) -> Optional[str]:
    if not terminal.is_capable(sys.stdin):
        return None

    def _default_indicator(index: int) -> str:
        return " [default]" if index == 0 else ""

    local_root_choices = [
        f"{i}. {local_root}{_default_indicator(i)}"
        for i, local_root in enumerate(local_roots)
    ]
    prompt = (
        "Enter the number of the local root you want to use "
        "(`Enter` for the default; any other key to quit): "
    )
    message = "\n".join(
        (
            "Would you like to run the command with a local root?",
            "",
            "Recently-used local roots:",
            *local_root_choices,
            prompt,
        )
    )

    LOG.info(message)

    try:
        user_input = input()
        index = 0 if user_input == "" else int(user_input)
        return local_roots[index]
    except (Exception, KeyboardInterrupt):
        LOG.error(f"No valid local root chosen. Quitting.")
        return None

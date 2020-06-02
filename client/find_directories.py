# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import logging
import os
from typing import Optional

from .filesystem import find_root


CONFIGURATION_FILE: str = ".pyre_configuration"
LOCAL_CONFIGURATION_FILE: str = ".pyre_configuration.local"
BINARY_NAME: str = "pyre.bin"
CLIENT_NAME: str = "pyre-client"


LOG: logging.Logger = logging.getLogger(__name__)


def find_project_root(original_directory: str) -> str:
    """Pyre always runs from the directory containing the nearest .pyre_configuration,
    if one exists."""
    global_root = find_root(original_directory, CONFIGURATION_FILE)
    return global_root or original_directory


def find_local_root(
    original_directory: str, local_root: Optional[str] = None
) -> Optional[str]:
    if local_root:
        return local_root

    global_root = find_root(original_directory, CONFIGURATION_FILE)
    local_root = find_root(original_directory, LOCAL_CONFIGURATION_FILE)

    # If the global configuration root is deeper than local configuration, ignore local.
    if global_root and local_root and global_root.startswith(local_root):
        local_root = None
    if local_root:
        return local_root


def _find_directory_upwards(base: str, target: str) -> Optional[str]:
    """
    Walk directories upwards from base, until the root directory is
    reached. At each step, check if the target directory exist, and return
    it if found. Return None if the search is unsuccessful.
    """
    while True:
        step = os.path.join(base, target)
        LOG.debug("Trying with: `%s`", step)
        if os.path.isdir(step):
            return step
        parent_directory = os.path.dirname(base)
        if parent_directory == base:
            # We have reached the root.
            break
        base = parent_directory
    return None


def find_typeshed() -> Optional[str]:
    override = os.getenv("PYRE_TYPESHED")
    if override:
        return override

    current_directory = os.path.dirname(os.path.realpath(__file__))

    # Prefer the typeshed we bundled ourselves (if any) to the one
    # from the environment.
    bundled_typeshed = _find_directory_upwards(
        current_directory, "pyre_check/typeshed/"
    )
    if bundled_typeshed:
        return bundled_typeshed

    try:
        import typeshed  # pyre-fixme: Can't find module import typeshed

        return typeshed.typeshed
    except ImportError:
        LOG.debug("`import typeshed` failed, attempting a manual lookup")

    # This is a terrible, terrible hack.
    return _find_directory_upwards(current_directory, "typeshed/")


def find_taint_models_directory() -> Optional[str]:
    return _find_directory_upwards(
        os.path.dirname(os.path.realpath(__file__)), "pyre_check/taint/"
    )

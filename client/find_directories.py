# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import multiprocessing
import os
import platform
import subprocess
import sys
import time
import traceback
from argparse import Namespace
from pathlib import Path
from typing import TYPE_CHECKING, Dict, Optional, Set, TextIO

from . import buck
from .exceptions import EnvironmentException
from .filesystem import find_root, translate_paths


CONFIGURATION_FILE: str = ".pyre_configuration"
LOCAL_CONFIGURATION_FILE: str = ".pyre_configuration.local"
BINARY_NAME: str = "pyre.bin"
CLIENT_NAME: str = "pyre-client"

if TYPE_CHECKING:
    from .configuration import Configuration


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
        _check_nested_configurations(local_root)
        return local_root

    global_root = find_root(original_directory, CONFIGURATION_FILE)
    local_root = find_root(original_directory, LOCAL_CONFIGURATION_FILE)
    # Check for illegal nested local configuration.
    _check_nested_configurations(local_root)

    # If the global configuration root is deeper than local configuration, ignore local.
    if global_root and local_root and global_root.startswith(local_root):
        local_root = None
    if local_root:
        return local_root


def _check_nested_configurations(local_root: Optional[str]) -> None:
    if local_root:
        parent_local_root = find_root(
            os.path.dirname(local_root.rstrip("/")), LOCAL_CONFIGURATION_FILE
        )
        if parent_local_root:
            LOG.warning(
                "Local configuration is nested under another local configuration at "
                "`{}`.\n   Please combine the sources into a single configuration or "
                "split the parent configuration to avoid inconsistent errors.".format(
                    parent_local_root
                )
            )


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

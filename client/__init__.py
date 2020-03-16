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
LOG_DIRECTORY: str = ".pyre"

if TYPE_CHECKING:
    from .configuration import Configuration


LOG: logging.Logger = logging.getLogger(__name__)


def assert_readable_directory(directory: str) -> None:
    if not os.path.isdir(directory):
        raise EnvironmentException("{} is not a valid directory.".format(directory))
    if not os.access(directory, os.R_OK):
        raise EnvironmentException("{} is not a readable directory.".format(directory))


def assert_writable_directory(directory: str) -> None:
    if not os.path.isdir(directory):
        raise EnvironmentException("{} is not a valid directory.".format(directory))
    if not os.access(directory, os.W_OK):
        raise EnvironmentException("{} is not a writable directory.".format(directory))


def readable_directory(directory: str) -> str:
    assert_readable_directory(directory)
    return directory


def is_capable_terminal(file: TextIO = sys.stderr) -> bool:
    """
    Determine whether we are connected to a capable terminal.
    """
    if not os.isatty(file.fileno()):
        return False
    terminal = os.getenv("TERM", "dumb")
    # Hardcoded list of non-capable terminals.
    return terminal not in ["dumb", "emacs"]


def get_binary_version(configuration: "Configuration") -> Optional[str]:
    status = subprocess.run(
        [configuration.binary, "-version"], stdout=subprocess.PIPE, text=True
    )
    if status.returncode == 0:
        return status.stdout.strip()
    else:
        return None


def find_project_root(original_directory: str) -> str:
    """Pyre always runs from the directory containing the nearest .pyre_configuration,
    if one exists."""
    global_root = find_root(original_directory, CONFIGURATION_FILE)
    return global_root or original_directory


def find_local_root(
    original_directory: str, local_root: Optional[str] = None
) -> Optional[str]:
    if local_root:
        check_nested_configurations(local_root)
        return local_root

    global_root = find_root(original_directory, CONFIGURATION_FILE)
    local_root = find_root(original_directory, LOCAL_CONFIGURATION_FILE)
    # Check for illegal nested local configuration.
    check_nested_configurations(local_root)

    # If the global configuration root is deeper than local configuration, ignore local.
    if global_root and local_root and global_root.startswith(local_root):
        local_root = None
    if local_root:
        return local_root


def check_nested_configurations(local_root: Optional[str]) -> None:
    if local_root:
        parent_local_root = find_root(
            os.path.dirname(local_root.rstrip("/")), LOCAL_CONFIGURATION_FILE
        )
        if parent_local_root:
            LOG.warning(
                "Local configuration is nested under another local configuration at "
                "`{}`.\nPlease combine the sources into a single configuration or "
                "split the parent configuration to avoid inconsistent errors.".format(
                    parent_local_root
                )
            )


def find_dot_pyre_directory(
    dot_pyre_directory: Optional[Path], current_directory: str
) -> Path:
    return dot_pyre_directory or Path(current_directory, LOG_DIRECTORY)


def find_log_directory(
    current_directory: str, local_configuration: Optional[str], dot_pyre_directory: str
) -> str:
    """Pyre outputs all logs to a .pyre directory that lives in the project root."""
    log_directory = dot_pyre_directory
    if local_configuration:
        # `log_directory` will never escape `.pyre/` because in `switch_root` we have
        # guaranteed that configurations are never deeper than local configurations
        relative = os.path.relpath(local_configuration, current_directory)
        log_directory = os.path.join(log_directory, relative)
    Path(log_directory).mkdir(parents=True, exist_ok=True)
    return log_directory


def _resolve_filter_paths(
    arguments: Namespace, configuration: "Configuration", original_directory: str
) -> Set[str]:
    filter_paths = set()
    if arguments.source_directories or arguments.targets:
        if arguments.source_directories:
            filter_paths.update(arguments.source_directories)
        if arguments.targets:
            filter_paths.update(
                [buck.presumed_target_root(target) for target in arguments.targets]
            )
    else:
        local_configuration_root = configuration.local_configuration_root
        if local_configuration_root:
            filter_paths = {local_configuration_root}
    return translate_paths(filter_paths, original_directory)


def number_of_workers() -> int:
    try:
        return max(multiprocessing.cpu_count() - 4, 1)
    except NotImplementedError:
        return 4


def log_statistics(
    category: str,
    arguments: Optional[Namespace] = None,
    configuration: Optional["Configuration"] = None,
    integers: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, Optional[str]]] = None,
    logger: Optional[str] = None,
) -> None:
    integers = integers or {}
    if "time" not in integers:
        integers["time"] = int(time.time())
    normals = normals or {}
    if configuration:
        # pyre-fixme[9]: normals has type `Dict[str, str]`; used as `Union[Dict[str,
        #  Optional[str]], Dict[str, str]]`.
        normals: Dict[str, str] = {**normals, "version": configuration.version_hash}
        if not logger:
            logger = configuration.logger
    if not logger:
        raise ValueError("Logger must either be given or in configuration")
    if arguments:
        # pyre-fixme[9]: normals has type `Optional[Dict[str, Optional[str]]]`; used
        #  as `Union[Dict[str, Optional[str]], Dict[str, str]]`.
        normals = {**normals, "arguments": str(arguments)}
    try:
        statistics = {
            "int": integers,
            "normal": {
                **normals,
                "command_line": " ".join(sys.argv),
                "host": platform.node() or "",
                "platform": platform.system() or "",
                "user": os.getenv("USER", ""),
            },
        }
        statistics = json.dumps(statistics).encode("ascii", "strict")
        subprocess.run([logger, category], input=statistics)
    except Exception:
        LOG.warning("Unable to log using `%s`", logger)
        LOG.info(traceback.format_exc())


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

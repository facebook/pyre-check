# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import multiprocessing
import os
import shlex
import subprocess
import sys
import traceback
from argparse import Namespace
from typing import Any, Dict, Optional

from . import buck
from .filesystem import SharedSourceDirectory


LOG = logging.getLogger(__name__)


SUCCESS = 0
FAILURE = 1

TEXT = "text"
JSON = "json"

CONFIGURATION_FILE = ".pyre_configuration"
BINARY_NAME = "pyre.bin"


class EnvironmentException(Exception):
    pass


def is_capable_terminal() -> bool:
    """
    Determine whether we are connected to a capable terminal.
    """
    if not os.isatty(sys.stderr.fileno()):
        return False
    terminal = os.getenv("TERM", "dumb")
    # Hardcoded list of non-capable terminals.
    return terminal not in ["dumb", "emacs"]


def get_version(configuration):
    override = os.getenv("PYRE_BINARY")
    if override:
        return "override: {}".format(override)

    configured = configuration.get_version_hash()
    if configured:
        return configured

    return "No version set"


def find_source_root(original_directory=None):
    if not original_directory:
        original_directory = os.getcwd()

    current_directory = original_directory
    while current_directory != "/":
        absolute = os.path.join(current_directory, CONFIGURATION_FILE)
        if os.path.isfile(absolute):
            return current_directory
        current_directory = os.path.dirname(current_directory)
    return original_directory


def switch_root(arguments) -> None:
    arguments.original_directory = os.getcwd()
    root = find_source_root()
    os.chdir(root)
    arguments.current_directory = root


def resolve_source_directories(arguments, configuration, prompt=True):
    source_directories = set(arguments.source_directory or [])
    targets = set(arguments.target or [])

    # Only read configuration if no arguments were provided.
    if not source_directories and not targets:
        source_directories = set(configuration.source_directories)
        targets = set(configuration.targets)
    else:
        LOG.warning("Setting up a .pyre_configuration file may reduce overhead.")

    source_directories.update(
        buck.generate_source_directories(targets, build=arguments.build, prompt=prompt)
    )

    if len(source_directories) == 0:
        raise EnvironmentException("No targets or link trees to analyze.")

    # Translate link trees if we switched directories earlier.
    current_directory = os.getcwd()
    if not arguments.original_directory.startswith(current_directory):
        return source_directories

    translation = arguments.original_directory[len(current_directory) + 1 :]
    if not translation:
        return source_directories

    def _translate(path):
        if os.path.exists(path):
            return path

        translated = os.path.join(translation, path)
        if os.path.exists(translated):
            return translated

        return path

    return {_translate(path) for path in source_directories}


def number_of_workers() -> int:
    try:
        return max(multiprocessing.cpu_count() - 4, 1)
    except NotImplementedError:
        return 4


def merge_source_directories(source_directories, isolate=False):
    if len(source_directories) == 0:
        raise EnvironmentException("No source directory found.")

    if len(source_directories) == 1:
        return source_directories.pop()

    filesystem = SharedSourceDirectory(source_directories, isolate)
    filesystem.prepare()
    return filesystem.get_root()


def log_statistics(
    category: str,
    arguments: Namespace,
    # this is typed as a Any because configuration imports __init__
    configuration: Any,
    ints: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, str]] = None,
) -> None:
    try:
        ints = ints or {}
        normals = normals or {}
        statistics = {
            "int": ints,
            "normal": {
                **normals,
                "arguments": str(arguments),
                "command_line": " ".join(sys.argv),
                "host": os.getenv("HOSTNAME") or "",
                "source_directory": str(arguments.source_directory or []),
                "target": str(arguments.target or []),
                "user": os.getenv("USER") or "",
                "version": str(configuration.get_version_hash()),
            },
        }
        subprocess.run(
            "{} {} {}".format(
                shlex.quote(configuration.logger),
                shlex.quote(category),
                shlex.quote(json.dumps(statistics)),
            ),
            shell=True,
        )
    except Exception:
        LOG.warning("Unable to log using `%s`", configuration.logger)
        LOG.info(traceback.format_exc())


def _find_directory_upwards(base: str, target: str) -> Optional[str]:
    """
    Walk directories upwards from base, until the root directory is
    reached. At each step, check if the target directory exist, and return
    it if found. Return None is the search is unsuccessful.
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
    current_directory = os.path.dirname(os.path.realpath(__file__))

    # Prefer the typeshed we bundled ourselves (if any) to the one
    # from the environment.
    bundled_typeshed = _find_directory_upwards(
        current_directory, "pyre_check/typeshed/stdlib/"
    )
    if bundled_typeshed:
        return bundled_typeshed

    try:
        import typeshed

        return os.path.join(typeshed.typeshed, "stdlib/")
    except ImportError:
        LOG.debug("`import typeshed` failed, attempting a manual lookup")

    # This is a terrible, terrible hack.
    return _find_directory_upwards(current_directory, "typeshed/stdlib/")

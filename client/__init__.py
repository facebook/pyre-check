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
from typing import Any, Dict, Optional, Sized

from . import buck
from .exceptions import EnvironmentException as EnvironmentException
from .filesystem import SharedAnalysisDirectory


LOG = logging.getLogger(__name__)


CONFIGURATION_FILE = ".pyre_configuration"
BINARY_NAME = "pyre.bin"


def assert_readable_directory(directory: str) -> None:
    if not os.path.isdir(directory):
        raise EnvironmentException("{} is not a valid directory.".format(directory))
    if not os.access(directory, os.R_OK):
        raise EnvironmentException("{} is not a readable directory.".format(directory))


def is_capable_terminal() -> bool:
    """
    Determine whether we are connected to a capable terminal.
    """
    if not os.isatty(sys.stderr.fileno()):
        return False
    terminal = os.getenv("TERM", "dumb")
    # Hardcoded list of non-capable terminals.
    return terminal not in ["dumb", "emacs"]


def get_binary_version(configuration) -> str:
    override = os.getenv("PYRE_BINARY")
    if override:
        return "override: {}".format(override)

    configured = configuration.version_hash
    if configured:
        return configured

    return "No version set"


def _find_configuration_root(
    original_directory: str, configuration_file: str
) -> Optional[str]:
    current_directory = original_directory
    while current_directory != "/":
        absolute = os.path.join(current_directory, configuration_file)
        if os.path.isfile(absolute):
            return current_directory
        current_directory = os.path.dirname(current_directory)
    return None


def switch_root(arguments) -> None:
    if arguments.local_configuration is not None:
        arguments.local_configuration = os.path.realpath(arguments.local_configuration)

    arguments.original_directory = os.getcwd()
    arguments.local_configuration_directory = _find_configuration_root(
        arguments.original_directory, CONFIGURATION_FILE + ".local"
    )
    global_root = _find_configuration_root(
        arguments.original_directory, CONFIGURATION_FILE
    )
    root = global_root or arguments.original_directory
    os.chdir(root)
    arguments.current_directory = root


def translate_path(root, path):
    if os.path.isabs(path):
        return path

    translated = os.path.join(root, path)
    if os.path.exists(translated):
        return os.path.realpath(translated)

    return path


def translate_arguments(commands, arguments):
    root = arguments.original_directory

    if arguments.command in [commands.Analyze]:
        if arguments.taint_models_path:
            arguments.taint_models_path = translate_path(
                root, arguments.taint_models_path
            )

    if arguments.logger:
        arguments.logger = translate_path(root, arguments.logger)


def resolve_analysis_directories(arguments, configuration, prompt: bool = True):
    analysis_directories = set(arguments.analysis_directory or [])
    targets = set(arguments.target or [])

    # Only read configuration if no arguments were provided.
    if not analysis_directories and not targets:
        analysis_directories = set(configuration.analysis_directories)
        targets = set(configuration.targets)
    else:
        LOG.warning("Setting up a .pyre_configuration file may reduce overhead.")

    analysis_directories.update(
        buck.generate_analysis_directories(
            targets, build=arguments.build, prompt=prompt
        )
    )
    if os.path.isfile(CONFIGURATION_FILE):
        initialization_command = "pyre init --local"
    else:
        initialization_command = "pyre init"
    if len(analysis_directories) == 0:
        raise EnvironmentException(
            "No targets or link trees to analyze.\n"
            "You can run `pyre init --local` to set up a local configuration, "
            "or `pyre --help` to check valid flags.".format(initialization_command)
        )

    # Translate link trees if we switched directories earlier.
    current_directory = os.getcwd()
    if not arguments.original_directory.startswith(current_directory):
        return analysis_directories

    translation = os.path.relpath(arguments.original_directory, current_directory)
    if not translation:
        return analysis_directories

    return {translate_path(translation, path) for path in analysis_directories}


def number_of_workers() -> int:
    try:
        return max(multiprocessing.cpu_count() - 4, 1)
    except NotImplementedError:
        return 4


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
                "analysis_directory": str(arguments.analysis_directory or []),
                "target": str(arguments.target or []),
                "user": os.getenv("USER") or "",
                "version": configuration.version_hash,
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

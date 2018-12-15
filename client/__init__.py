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
import traceback
from argparse import Namespace
from typing import Any, Dict, Optional

from . import buck
from .exceptions import EnvironmentException
from .filesystem import AnalysisDirectory, SharedAnalysisDirectory  # noqa


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


def find_configuration_root(
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
    arguments.original_directory = os.getcwd()
    local_root = find_configuration_root(
        arguments.original_directory, CONFIGURATION_FILE + ".local"
    )
    global_root = find_configuration_root(
        arguments.original_directory, CONFIGURATION_FILE
    )

    # If the global configuration root is deeper than local configuration, ignore local.
    if global_root and local_root and global_root.startswith(local_root):
        local_root = None
    if local_root and arguments.local_configuration is None:
        arguments.local_configuration = local_root

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


def translate_paths(paths, original_directory):
    current_directory = os.getcwd()
    if not original_directory.startswith(current_directory):
        return paths
    translation = os.path.relpath(original_directory, current_directory)
    if not translation:
        return paths
    return {translate_path(translation, path) for path in paths}


def _resolve_source_directories(arguments, commands, configuration, prompt):
    source_directories = set(arguments.source_directories or [])
    targets = set(arguments.targets or [])

    # Only read configuration if no arguments were provided.
    if not source_directories and not targets:
        source_directories = set(configuration.source_directories)
        targets = set(configuration.targets)
    else:
        LOG.warning(
            "Setting up a `.pyre_configuration` with `pyre init` may reduce overhead "
        )

    always_build = arguments.build or arguments.command in [
        commands.Restart,
        commands.Start,
    ]
    source_directories.update(
        buck.generate_source_directories(targets, build=always_build, prompt=prompt)
    )
    if len(source_directories) == 0:
        raise EnvironmentException("No targets or source directories to analyze.")

    # Translate link trees if we switched directories earlier.
    return translate_paths(source_directories, arguments.original_directory)


def _resolve_filter_paths(arguments, configuration):
    filter_paths = []
    if arguments.source_directories or arguments.targets:
        if arguments.source_directories:
            filter_paths += arguments.source_directories
        if arguments.targets:
            filter_paths += [
                buck.presumed_target_root(target) for target in arguments.targets
            ]
    else:
        local_configuration_root = configuration.local_configuration_root
        if local_configuration_root:
            filter_paths = [local_configuration_root]
    return translate_paths(filter_paths, arguments.original_directory)


def resolve_analysis_directory(
    arguments, commands, configuration, isolate: bool = False, prompt: bool = True
):
    source_directories = _resolve_source_directories(
        arguments, commands, configuration, prompt
    )
    if arguments.filter_directory:
        filter_paths = [arguments.filter_directory]
    else:
        filter_paths = _resolve_filter_paths(arguments, configuration)
    local_configuration_root = configuration.local_configuration_root
    if local_configuration_root:
        local_configuration_root = os.path.relpath(
            local_configuration_root, arguments.current_directory
        )

    if len(source_directories) == 1:
        analysis_directory = AnalysisDirectory(source_directories.pop(), filter_paths)
    else:
        analysis_directory = SharedAnalysisDirectory(
            source_directories, filter_paths, local_configuration_root, isolate
        )
    return analysis_directory


def number_of_workers() -> int:
    try:
        return max(multiprocessing.cpu_count() - 4, 1)
    except NotImplementedError:
        return 4


def log_statistics(
    category: str,
    arguments: Optional[Namespace] = None,
    # this is typed as a Any because configuration imports __init__
    configuration: Optional[Any] = None,
    integers: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, str]] = None,
    logger: Optional[str] = None,
) -> None:
    integers = integers or {}
    normals = normals or {}
    if configuration:
        normals = {**normals, "version": configuration.version_hash}
        if not logger:
            logger = configuration.logger
    if not logger:
        raise ValueError("Logger must either be given or in configuration")
    if arguments:
        normals = {
            **normals,
            "source_directories": str(arguments.source_directories or []),
            "arguments": str(arguments),
            "target": str(arguments.targets or []),
        }
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
        subprocess.run(
            [logger, category], input=json.dumps(statistics), encoding="ascii"
        )
    except Exception:
        LOG.warning("Unable to log using `%s`", logger)
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

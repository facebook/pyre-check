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

from . import buck, buck_project_builder
from .exceptions import EnvironmentException
from .filesystem import (  # noqa
    AnalysisDirectory,
    SharedAnalysisDirectory,
    find_root,
    translate_path,
    translate_paths,
)


LOG = logging.getLogger(__name__)


CONFIGURATION_FILE = ".pyre_configuration"
BINARY_NAME = "pyre.bin"


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


def is_capable_terminal(file=sys.stderr) -> bool:
    """
    Determine whether we are connected to a capable terminal.
    """
    if not os.isatty(file.fileno()):
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


def switch_root(arguments) -> None:
    arguments.original_directory = os.getcwd()
    local_root = find_root(arguments.original_directory, CONFIGURATION_FILE + ".local")
    global_root = find_root(arguments.original_directory, CONFIGURATION_FILE)

    # If the global configuration root is deeper than local configuration, ignore local.
    if global_root and local_root and global_root.startswith(local_root):
        local_root = None
    if local_root and arguments.local_configuration is None:
        arguments.local_configuration = local_root

    root = global_root or arguments.original_directory
    os.chdir(root)
    arguments.current_directory = root


def translate_arguments(commands, arguments):
    root = arguments.original_directory

    if arguments.command in [commands.Analyze]:
        if arguments.taint_models_path:
            arguments.taint_models_path = translate_path(
                root, arguments.taint_models_path
            )

    if arguments.logger:
        arguments.logger = translate_path(root, arguments.logger)


def _buck_target_count(arguments, configuration):
    if arguments.source_directories or arguments.targets:
        return len(set(arguments.targets or []))
    else:
        return len(set(configuration.targets or []))


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
) -> AnalysisDirectory:
    # Only read from the configuration if no explicit targets are passed in.
    if not arguments.source_directories and not arguments.targets:
        source_directories = configuration.source_directories
        targets = configuration.targets
    else:
        source_directories = arguments.source_directories or []
        targets = arguments.targets or []
        if targets:
            configuration_name = ".pyre_configuration.local"
            command = "pyre init --local"
        else:
            configuration_name = ".pyre_configuration"
            command = "pyre init"
        LOG.warning(
            "Setting up a `%s` with `%s` may reduce overhead.",
            configuration_name,
            command,
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

    use_buck_builder = arguments.use_buck_builder or configuration.use_buck_builder
    ignore_unbuilt_dependencies = use_buck_builder and (
        arguments.ignore_unbuilt_dependencies
        or configuration.ignore_unbuilt_dependencies
    )

    if len(source_directories) == 1 and len(targets) == 0:
        analysis_directory = AnalysisDirectory(
            source_directories.pop(),
            filter_paths=filter_paths,
            search_path=configuration.search_path,
        )
    else:
        build = arguments.build or arguments.command in [
            commands.Check,
            commands.Restart,
        ]
        buck_builder = buck.SimpleBuckBuilder(build=build, prompt=prompt)
        if use_buck_builder:
            buck_root = buck.find_buck_root(os.getcwd())
            if not buck_root:
                raise EnvironmentException(
                    "No Buck configuration at `{}` or any of its ancestors.".format(
                        os.getcwd()
                    )
                )
            buck_builder = buck_project_builder.FastBuckBuilder(
                buck_root, fail_on_unbuilt_target=not ignore_unbuilt_dependencies
            )
        else:
            buck_builder = buck.SimpleBuckBuilder(build=build, prompt=prompt)

        analysis_directory = SharedAnalysisDirectory(
            source_directories=source_directories,
            targets=targets,
            buck_builder=buck_builder,
            original_directory=arguments.original_directory,
            filter_paths=filter_paths,
            local_configuration_root=local_configuration_root,
            extensions=configuration.extensions,
            search_path=configuration.search_path,
            isolate=isolate,
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
        normals = {
            **normals,
            "version": configuration.version_hash,
        }  # type: Dict[str, str]
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

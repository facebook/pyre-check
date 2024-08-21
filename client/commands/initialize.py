# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module contains logic for `pyre init`, which interactively
helps a user create a new pyre configuration.
"""

import json
import logging
import os
import shutil
import subprocess
import sys
from logging import Logger
from pathlib import Path
from typing import Any, Dict, Optional, Tuple, Union

from .. import log
from ..find_directories import (
    BINARY_NAME,
    CONFIGURATION_FILE,
    find_global_root,
    find_parent_directory_containing_file,
    find_taint_models_directories,
    find_typeshed,
    LOCAL_CONFIGURATION_FILE,
)
from . import commands

LOG: Logger = logging.getLogger(__name__)


class InitializationException(Exception):
    pass


def _create_source_directory_element(source: str) -> Union[str, Dict[str, str]]:
    if source == ".":
        return source
    if not Path(source).is_dir():
        raise InitializationException(f"No directory found at `{source}`.")
    # Imports are likely relative to the parent of the package.
    package_root = find_parent_directory_containing_file(Path(source), "__init__.py")
    if package_root is not None:
        return {
            "import_root": os.path.relpath(str(package_root.parent), "."),
            "source": source,
        }
    else:
        return source


def _check_configuration_file_location(
    configuration_path: Path, current_directory: Path, global_root: Optional[Path]
) -> None:
    if os.path.isfile(configuration_path):
        if global_root:
            error = (
                "Local configurations must be created in subdirectories of "
                + f"`{str(current_directory)}` as it already contains a "
                + "`.pyre_configuration`."
            )
        else:
            error = (
                "A pyre configuration already exists at "
                + f"`{str(configuration_path)}`."
            )
        raise InitializationException(error)
    local_configuration_path = current_directory / LOCAL_CONFIGURATION_FILE
    if local_configuration_path.is_file():
        raise InitializationException(
            "A local pyre configuration already exists at "
            + f"`{str(local_configuration_path)}`."
        )


def _get_local_configuration(
    current_directory: Path, buck_root: Optional[Path]
) -> Dict[str, Any]:
    configuration: Dict[str, Any] = {}
    using_targets = log.get_yes_no_input("Is your project built with Buck?")
    if using_targets:
        targets = log.get_input(
            "Which buck target(s) should pyre analyze?\n"
            + "  Default: Analyze all targets under the configuration (assume fbcode).\n"
            + "  (Ex. `fbcode//target:a, fbsource//target/b/...`)\n"
        ).strip()
        if len(targets) == 0:
            if buck_root:
                root = current_directory.relative_to(buck_root)
                configuration["targets"] = [f"fbcode//{str(root)}/..."]
            else:
                raise InitializationException(
                    "No `.buckconfig` found with which to create a default target."
                )
        else:
            configuration["targets"] = [target.strip() for target in targets.split(",")]
    else:
        source_directories = log.get_input(
            "Which directory(ies) should pyre analyze?\n"
        )
        configuration["source_directories"] = [
            directory.strip() for directory in source_directories.split(",")
        ]

    # TODO(T132432706): Ask for oncall in global configuration, but not in OSS.
    oncall = log.get_input("What oncall is responsible for this project?\n").strip()
    if oncall:
        configuration["oncall"] = oncall
    return configuration


def _create_watchman_configuration() -> None:
    watchman_configuration_path = os.path.abspath(".watchmanconfig")
    watchman_path = shutil.which("watchman")
    if watchman_path is not None and log.get_yes_no_input(
        "Also initialize watchman in the current directory?"
    ):
        try:
            if not os.path.isfile(watchman_configuration_path):
                with open(watchman_configuration_path, "w+") as configuration_file:
                    configuration_file.write("{}\n")
                LOG.warning(
                    "Created basic `.watchmanconfig` at "
                    + f"{watchman_configuration_path}"
                )
            subprocess.run(
                [watchman_path, "watch-project", "."],
                check=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                universal_newlines=True,
            )
            LOG.warning("Current directory is being watched by `watchman`.")
        except IsADirectoryError:
            LOG.warning(f"Unable to write to {watchman_configuration_path}.")
        except subprocess.CalledProcessError:
            LOG.warning("Failed to run `watchman watch-project .`.")


def _get_configuration(
    taint_models_directory_required: Optional[bool] = False,
) -> Dict[str, Any]:
    configuration: Dict[str, Any] = {}

    _create_watchman_configuration()
    binary_path = shutil.which(BINARY_NAME)
    if binary_path is None:
        binary_path = shutil.which(
            os.path.join(os.path.dirname(sys.argv[0]), BINARY_NAME)
        )
    if binary_path is None:
        binary_path = os.path.abspath(
            log.get_input(f"No `{BINARY_NAME}` found, enter the path manually: ")
        )
        if not os.path.isfile(binary_path):
            raise InitializationException(
                f"Unable to locate binary at `{binary_path}`."
            )
        configuration["binary"] = binary_path
    else:
        LOG.info(f"Binary found at `{binary_path}`")

    typeshed: Optional[Path] = find_typeshed()
    if typeshed is None:
        typeshed = Path(
            log.get_input("Unable to locate typeshed, please enter its root: ")
        ).resolve()
        if not typeshed.is_dir():
            raise InitializationException(
                f"No typeshed directory found at `{typeshed}`."
            )
        configuration["typeshed"] = str(typeshed)
    else:
        LOG.info(f"Typeshed found at `{typeshed}``")

    if taint_models_directory_required:
        taint_models_paths = find_taint_models_directories()
        if taint_models_paths is None:
            taint_models_paths = [
                Path(
                    log.get_input(
                        "Unable to find taint models directory, please enter its root: "
                    )
                ).resolve()
            ]
        configuration["taint_models_path"] = [str(path) for path in taint_models_paths]

    source_directory_input = log.get_optional_input(
        "Which directory(ies) should pyre analyze?", "."
    )
    source_directory_paths = [
        directory.strip() for directory in source_directory_input.split(",")
    ]
    configuration["source_directories"] = [
        _create_source_directory_element(path) for path in source_directory_paths
    ]

    LOG.info(
        "Pyre will automatically add typed pacakages installed on your system "
        "as type checking dependencies, according to PEP561. You can edit the "
        "configuration file if you want to change the behavior. "
    )
    configuration["site_package_search_strategy"] = "pep561"
    return configuration


def get_configuration_and_path(
    taint_models_directory_required: Optional[bool] = False,
) -> Tuple[Dict[str, Any], Path]:
    global_root: Optional[Path] = find_global_root(Path("."))
    buck_root: Optional[Path] = find_parent_directory_containing_file(
        Path("."), ".buckconfig"
    )
    current_directory: Path = Path(os.getcwd())
    configuration_path = current_directory / CONFIGURATION_FILE
    _check_configuration_file_location(
        configuration_path, current_directory, global_root
    )
    local_configuration_path = current_directory / LOCAL_CONFIGURATION_FILE
    if global_root:
        configuration_path = local_configuration_path
        configuration = _get_local_configuration(current_directory, buck_root)
    else:
        configuration = _get_configuration(taint_models_directory_required)
    return configuration, configuration_path


def write_configuration(
    configuration: Dict[str, Any], configuration_path: Path
) -> None:
    with open(configuration_path, "w+") as configuration_file:
        json.dump(configuration, configuration_file, sort_keys=True, indent=2)
        configuration_file.write("\n")


def run() -> commands.ExitCode:
    try:
        configuration, configuration_path = get_configuration_and_path()
        write_configuration(configuration, configuration_path)
        LOG.log(
            log.SUCCESS,
            "Successfully initialized pyre!\n"
            + f"  You can view the configuration at `{configuration_path}`.\n"
            + "  You can now run the type checker with `pyre`.",
        )
        return commands.ExitCode.SUCCESS
    except InitializationException as error:
        LOG.error(f"{error}")
        return commands.ExitCode.FAILURE

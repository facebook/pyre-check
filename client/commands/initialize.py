# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import shutil
import subprocess
import sys
from logging import Logger
from pathlib import Path
from typing import Any, Dict, Optional, Union

from .. import log
from ..find_directories import (
    BINARY_NAME,
    CONFIGURATION_FILE,
    LOCAL_CONFIGURATION_FILE,
    find_global_root,
    find_parent_directory_containing_file,
    find_taint_models_directory,
    find_typeshed,
)
from . import commands

LOG: Logger = logging.getLogger(__name__)


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


class InitializationException(Exception):
    pass


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


def _get_configuration() -> Dict[str, Any]:
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

    taint_models_path = find_taint_models_directory()
    if taint_models_path is not None:
        configuration["taint_models_path"] = str(taint_models_path)

    source_directory_input = log.get_optional_input(
        "Which directory(ies) should pyre analyze?", "."
    )
    source_directory_paths = [
        directory.strip() for directory in source_directory_input.split(",")
    ]
    configuration["source_directories"] = [
        _create_source_directory_element(path) for path in source_directory_paths
    ]
    return configuration


def _get_local_configuration(
    current_directory: Path, buck_root: Optional[Path]
) -> Dict[str, Any]:
    configuration: Dict[str, Any] = {}
    using_targets = log.get_yes_no_input("Is your project built with Buck?")
    if using_targets:
        targets = log.get_input(
            "Which buck target(s) should pyre analyze?\n"
            + "  Default: Analyze all targets under the configuration.\n"
            + "  (Ex. `//target:a, //target/b/...`)\n"
        ).strip()
        if len(targets) == 0:
            if buck_root:
                root = current_directory.relative_to(buck_root)
                configuration["targets"] = [f"//{str(root)}/..."]
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
    return configuration


def run() -> commands.ExitCode:
    try:
        global_root: Optional[Path] = find_global_root(Path("."))
        buck_root: Optional[Path] = find_parent_directory_containing_file(
            Path("."), ".buckconfig"
        )
        current_directory: Path = Path(os.getcwd())
        configuration_path = current_directory / CONFIGURATION_FILE
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
        if global_root:
            configuration_path = local_configuration_path
            configuration = _get_local_configuration(current_directory, buck_root)
        else:
            configuration = _get_configuration()

        with open(configuration_path, "w+") as configuration_file:
            json.dump(configuration, configuration_file, sort_keys=True, indent=2)
            configuration_file.write("\n")
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

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
from typing import Any, Dict, Optional

from .. import log
from ..find_directories import (
    BINARY_NAME,
    CONFIGURATION_FILE,
    find_global_root,
    find_taint_models_directory,
    find_typeshed,
)
from .command import CommandParser


LOG: Logger = logging.getLogger(__name__)


class InitializationException(Exception):
    pass


class Initialize(CommandParser):
    NAME = "initialize"

    def __init__(self) -> None:
        super().__init__()

    def _get_configuration(self) -> Dict[str, Any]:
        configuration: Dict[str, Any] = {}

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

        taint_models_path = find_taint_models_directory()
        if taint_models_path is not None:
            configuration["taint_models_path"] = str(taint_models_path)

        analysis_directory = log.get_optional_input(
            "Which directory should pyre be initialized in?", "."
        )

        configuration["source_directories"] = [analysis_directory]
        return configuration

    def _get_local_configuration(self) -> Dict[str, Any]:
        configuration: Dict[str, Any] = {}
        using_targets = log.get_yes_no_input("Is your project built with Buck?")
        if using_targets:
            targets = log.get_input(
                "Which buck target(s) should pyre analyze? \
                (`//target:a`, `//target/b/...`)\n"
            )
            configuration["targets"] = [target.strip() for target in targets.split(",")]
        else:
            source_directories = log.get_input(
                "Which directory(ies) should pyre analyze?\n"
            )
            configuration["source_directories"] = [
                directory.strip() for directory in source_directories.split(",")
            ]
        return configuration

    def _is_local(self) -> bool:
        return find_global_root(Path(".")) is not None

    def _run(self) -> None:
        try:
            is_local = self._is_local()
            current_directory = os.getcwd()
            configuration_path = os.path.join(current_directory, CONFIGURATION_FILE)
            if os.path.isfile(configuration_path):
                if is_local:
                    error = (
                        "Local configurations must be created in subdirectories of "
                        + f"`{current_directory}` as it already contains a "
                        + "`.pyre_configuration`."
                    )
                else:
                    error = (
                        "A pyre configuration already exists at "
                        + f"`{configuration_path}`."
                    )
                raise InitializationException(error)
            local_configuration_path = configuration_path + ".local"
            if os.path.isfile(local_configuration_path):
                raise InitializationException(
                    "A local pyre configuration already exists at "
                    + f"`{local_configuration_path}`."
                )
            if is_local:
                configuration_path = configuration_path + ".local"
                configuration = self._get_local_configuration()
            else:
                configuration = self._get_configuration()

            with open(configuration_path, "w+") as configuration_file:
                json.dump(configuration, configuration_file, sort_keys=True, indent=2)
                configuration_file.write("\n")
            LOG.log(
                log.SUCCESS,
                "Successfully initialized pyre! "
                + f"You can view the configuration at `{configuration_path}`.",
            )
        except InitializationException as error:
            LOG.error(f"{error}")

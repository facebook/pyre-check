# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import shutil
import subprocess
from typing import Any, Dict

from .. import BINARY_NAME, CONFIGURATION_FILE, EnvironmentException, find_typeshed, log
from .command import Command


LOG = logging.getLogger(__name__)


class Initialize(Command):
    NAME = "initialize"

    def __init__(self, arguments, configuration, analysis_directory) -> None:
        self._local = arguments.local  # type: bool
        super(Initialize, self).__init__(arguments, configuration, analysis_directory)

    def _get_configuration(self) -> Dict[str, Any]:
        configuration = {}  # type: Dict[str, Any]

        watchman_configuration_path = os.path.abspath(".watchmanconfig")
        if shutil.which("watchman") is not None and log.get_yes_no_input(
            "Also initialize a watchman configuration?"
        ):
            try:
                with open(watchman_configuration_path, "w+") as configuration_file:
                    configuration_file.write("{}\n")
                subprocess.check_call(["watchman", "watch-project", "."])
            except (IsADirectoryError, subprocess.CalledProcessError):
                LOG.warning("Unable to initialize watchman for the current directory.")

        binary_path = shutil.which(BINARY_NAME)
        if binary_path is None:
            binary_path = os.path.abspath(
                log.get_input(
                    "No {} found, enter the path manually: ".format(BINARY_NAME)
                )
            )
            if not os.path.isfile(binary_path):
                raise EnvironmentException(
                    "Unable to locate binary at {}.".format(binary_path)
                )
        else:
            LOG.info("Binary found at {}".format(binary_path))
        configuration["binary"] = binary_path

        typeshed = find_typeshed()
        if typeshed is None:
            typeshed = os.path.abspath(
                log.get_input("Unable to locate typeshed, please enter its root: ")
            )
            if not os.path.isdir(typeshed):
                raise EnvironmentException(
                    "No typeshed directory found at {}.".format(typeshed)
                )
        configuration["typeshed"] = typeshed

        analysis_directory = log.get_optional_input(
            "Which directory should pyre be initialized in?", "."
        )

        configuration["source_directories"] = [analysis_directory]
        return configuration

    def _get_local_configuration(self) -> Dict[str, Any]:
        configuration = {}
        targets = log.get_input(
            "Which buck target(s) should pyre analyze? (//target:a,//target/b/...) "
        )
        configuration["targets"] = [target.strip() for target in targets.split(",")]
        push_blocking = log.get_yes_no_input(
            "Would you like pyre to enable pyre's push blocking integration?"
        )
        configuration["push_blocking"] = push_blocking
        if push_blocking:
            configuration["differential"] = log.get_yes_no_input(
                "Should pyre only be push-blocking on newly introduced errors?"
            )
        return configuration

    def _run(self) -> None:
        configuration_path = os.path.join(self._original_directory, CONFIGURATION_FILE)
        if os.path.isfile(configuration_path):
            raise EnvironmentException(
                "A pyre configuration already exists at {}.".format(configuration_path)
            )
        if os.path.isfile(configuration_path + ".local"):
            raise EnvironmentException(
                "A local pyre configuration already exists at {}.".format(
                    configuration_path + ".local"
                )
            )
        if self._local:
            configuration_path = configuration_path + ".local"
            configuration = self._get_local_configuration()
        else:
            configuration = self._get_configuration()

        with open(configuration_path, "w+") as configuration_file:
            json.dump(configuration, configuration_file, sort_keys=True, indent=2)
        LOG.info(
            "Successfully initialized pyre! "
            + "You can view the configuration at `{}`.".format(configuration_path)
        )

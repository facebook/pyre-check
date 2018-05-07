# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import shutil
import json

from .. import (
    BINARY_NAME,
    CONFIGURATION_FILE,
    EnvironmentException,
    find_typeshed,
    log)
from .command import Command

LOG = logging.getLogger(__name__)


class Initialize(Command):
    NAME = 'initialize'

    def _run(self) -> None:
        configuration_path = os.path.join(os.getcwd(), CONFIGURATION_FILE)
        if os.path.isfile(configuration_path):
            raise EnvironmentException(
                "A {} already exists at {}.".format(
                    CONFIGURATION_FILE,
                    configuration_path))

        configuration = {}

        binary_path = shutil.which(BINARY_NAME)
        if binary_path is None:
            binary_path = os.path.abspath(
                log.get_input(
                    'No {} found, enter the path manually: '.format(
                        BINARY_NAME)))
            if not os.path.isfile(binary_path):
                raise EnvironmentException(
                    "Unable to locate binary at {}.".format(binary_path))
        else:
            LOG.info("Binary found at {}".format(binary_path))
        configuration["binary"] = binary_path

        typeshed = find_typeshed()
        if typeshed is None:
            typeshed = os.path.abspath(
                log.get_input(
                    'Unable to locate typeshed, please enter its root: '
                ))
            if not os.path.isdir(typeshed):
                raise EnvironmentException(
                    "No typeshed directory found at {}.".format(typeshed))
        configuration["typeshed"] = typeshed

        configuration["source_directories"] = ["."]
        with open(configuration_path, "w+") as configuration_file:
            json.dump(
                configuration,
                configuration_file,
                sort_keys=True,
                indent=2)
        LOG.info(
            "Successfully initialized pyre! " +
            "You can view the configuration at {}".format(configuration_path))

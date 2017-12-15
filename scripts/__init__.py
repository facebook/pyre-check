# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import shlex
import subprocess
import traceback
from tools.pyre.scripts import (
    buck,
)


LOG = logging.getLogger(__name__)


SUCCESS = 0
FAILURE = 1

TEXT = 'text'
JSON = 'json'

CONFIGURATION_FILE = '.pyre_configuration'


class EnvironmentException(Exception):
    pass


def get_version(configuration):
    override = os.getenv('PYRE_BINARY')
    if override:
        return 'override: {}'.format(override)

    configured = configuration.get_version_hash()
    if configured:
        return configured

    return 'No version set'


def find_project_root(original_directory=os.getcwd()):
    current_directory = original_directory
    while current_directory != "/":
        absolute = os.path.join(current_directory, CONFIGURATION_FILE)
        if os.path.isfile(absolute):
            return current_directory
        current_directory = os.path.dirname(current_directory)
    return original_directory


def switch_root(arguments):
    arguments.original_directory = os.getcwd()
    root = find_project_root()
    os.chdir(root)
    arguments.current_directory = root


def resolve_link_trees(arguments, configuration):
    link_trees = set(configuration.link_trees)
    if arguments.link_tree:
        link_trees = set(arguments.link_tree)

    link_trees.update(
        buck.generate_link_trees(
            arguments,
            arguments.target or configuration.targets))

    if len(link_trees) == 0:
        raise EnvironmentException(
            "No targets or link trees to analyze.")

    # Translate link trees if we switched directories earlier.
    current_directory = os.getcwd()
    if not arguments.original_directory.startswith(current_directory):
        return link_trees

    translation = arguments.original_directory[len(current_directory) + 1:]
    if not translation:
        return link_trees

    def _translate(path):
        if os.path.exists(path):
            return path

        translated = os.path.join(translation, path)
        if os.path.exists(translated):
            return translated

        return path

    return {_translate(path) for path in link_trees}


def log_to_scuba(table_name, logger, sample):
    try:
        subprocess.run(
            "{} {} {}".format(
                shlex.quote(logger),
                shlex.quote(table_name),
                shlex.quote(json.dumps(sample)),
            ),
            shell=True
        )
    except Exception:
        LOG.warning('Unable to log using `%s`', logger)
        LOG.info(traceback.format_exc())

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
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


def is_disabled(configuration):
    return configuration.disabled


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
    link_trees = set(arguments.link_tree or [])
    link_trees.update(configuration.link_trees)

    if arguments.target:
        link_trees.update(
            buck.generate_link_trees(
                arguments, configuration.targets + arguments.target))

    if len(link_trees) == 0:
        raise EnvironmentException(
            "No targets or link trees to analyze.")
    return link_trees

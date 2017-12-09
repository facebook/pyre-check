# Copyright 2004-present Facebook.  All rights reserved.

import glob
import logging
import os
import subprocess
import sys


LOG = logging.getLogger(__name__)


SUCCESS = 0
FAILURE = 1

TEXT = 'text'
JSON = 'json'

CONFIGURATION_FILE = '.pyre_configuration'


class EnvironmentException(Exception):
    pass


def get_binary(configuration):
    override = os.getenv('PYRE_BINARY')
    if override:
        if not os.path.exists(override):
            raise EnvironmentException(
                "Binary override `{}` does not exist.".format(override))
        return override

    return os.path.join(
        '/mnt/dewey/fbsource/.commits/',
        get_version(configuration),
        'fbcode/builds/pyre/main.native')


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
            _buck_link_trees(arguments.target + configuration.targets))

    return link_trees


def _buck_link_trees(arguments):
    """Get link tree link_tree for a given buck target."""
    if not arguments.target:
        return []

    link_trees = []

    for target in arguments.target:
        if arguments.normalize:
            LOG.info('Normalizing target `%s`', target)
            try:
                targets = subprocess.check_output(
                    ['buck', 'targets', target],
                    stderr=subprocess.DEVNULL).decode().strip().split('\n')
            except subprocess.CalledProcessError:
                raise EnvironmentException(
                    'Could not normalize target `{}`.'.format(target))

        if arguments.build:
            LOG.info('Building `%s`', target)
            build_command = ['buck', 'build']
            for target in targets:
                build_command.append(target)
            try:
                subprocess.check_output(
                    build_command,
                    stderr=sys.stdout.buffer)
            except subprocess.CalledProcessError:
                raise EnvironmentException(
                    'Could not build target `{}`.'.format(target))

        target_path = target
        if target_path.startswith('//'):
            target_path = target_path[2:]
        target_path = target_path.replace(':', '/')

        generated_link_trees = glob.glob(
            os.path.join('buck-out/gen/', target_path + '#*link-tree'))

        if len(generated_link_trees) != 1 and \
                arguments.normalize and \
                arguments.build:
            raise EnvironmentException(
                'Could not find link trees for `{}`. The target might not be '
                'normalized or built. See `{} --help` for more '
                'information.'.format(target, sys.argv[0]))
        elif len(generated_link_trees) != 1:
            LOG.error(
                'Could not find link trees for `%s`. '
                'The target might not be normalized or built.',
                target)
            sys.stderr.write("Normalize and build target? [Y/n] ")
            choice = input().strip().lower()
            if choice in ['', 'y', 'ye', 'yes']:
                arguments.normalize = True
                arguments.build = True
                link_trees.extend(_buck_link_trees(arguments))
            else:
                sys.exit(0)
        else:
            link_trees.append(generated_link_trees[0])
    return link_trees

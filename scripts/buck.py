# Copyright 2004-present Facebook.  All rights reserved.

import glob
import logging
import os
import subprocess
import sys
from collections import namedtuple

LOG = logging.getLogger(__name__)


BuckOut = namedtuple('BuckOut', 'link_trees targets_not_found')


class BuckException(Exception):
    pass


def _find_link_trees(targets):
    targets_not_found = []
    link_trees = []
    for target_path in targets:
        if target_path.startswith('//'):
            target_path = target_path[2:]
        target_path = target_path.replace(':', '/')

        built_targets = glob.glob(
            os.path.join('buck-out/gen/', target_path + '*'))
        if (not target_path.endswith(('lib', 'library')) and
                len(built_targets) == 0):
            targets_not_found.append(target_path)
        link_trees.extend(glob.glob(
            os.path.join('buck-out/gen/', target_path + '#*link-tree')))
    return BuckOut(link_trees, targets_not_found)


def _normalize(target):
    LOG.info('Normalizing target `%s`', target)
    try:
        targets = subprocess.check_output(
            ['buck', 'targets', target],
            stderr=subprocess.DEVNULL).decode().strip().split('\n')
        return targets
    except subprocess.CalledProcessError:
        raise BuckException(
            'Could not normalize target `{}`.'.format(target))


def _build_targets(original_target_name, targets):
    LOG.info('Building `%s`', original_target_name)
    command = ['buck', 'build']
    command.extend(targets)
    try:
        subprocess.check_output(
            command,
            stderr=subprocess.DEVNULL)
    except subprocess.CalledProcessError:
        raise BuckException(
            'Could not build target `{}`.'.format(original_target_name))


def _get_yes_no_input(prompt):
    LOG.warning(prompt + ' [Y/n] ')
    choice = input().strip().lower()
    return choice in ['', 'y', 'ye', 'yes']


def generate_link_trees(arguments, original_targets):
    buck_out = _find_link_trees(original_targets)
    link_trees = buck_out.link_trees

    if len(buck_out.targets_not_found) > 0:
        LOG.warning(
            'Passing in normalized buck targets will reduce runtime.\n   '
            'You can set up a .pyre_configuration file to reduce overhead.')

    for target in buck_out.targets_not_found:
        targets = _normalize(target)

        if arguments.build:
            _build_targets(target, targets)

        buck_out = _find_link_trees(targets)
        if len(buck_out.link_trees) == 0 and arguments.build:
            raise BuckException(
                'Could not find link trees for `{}`. See `{} --help` for more '
                'information.'.format(target, sys.argv[0]))
        elif len(buck_out.link_trees) == 0:
            LOG.error(
                'Could not find link trees for `%s`. '
                'The target might not be built.',
                target)
            if _get_yes_no_input("Build target?"):
                arguments.build = True
                return generate_link_trees(arguments, original_targets)
            raise BuckException(
                'Could not find link trees for `{}`. See `{} --help` for more '
                'information.'.format(target, sys.argv[0]))
        elif len(buck_out.targets_not_found) > 0 and not arguments.build:
            LOG.error(
                'Could not find link trees for all targets in `%s`. '
                'The target may only be partially built.',
                target)
            LOG.error(
                'Potentially unbuilt subtargets:\n   %s',
                '\n   '.join(buck_out.targets_not_found))
            if _get_yes_no_input("Re-build target?"):
                arguments.build = True
                return generate_link_trees(arguments, original_targets)
            else:
                link_trees.extend(buck_out.link_trees)
        else:
            link_trees.extend(buck_out.link_trees)
    return link_trees

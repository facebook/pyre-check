# Copyright 2004-present Facebook.  All rights reserved.

import glob
import logging
import os
import subprocess
import sys
from collections import namedtuple
from . import log

LOG = logging.getLogger(__name__)


BuckOut = namedtuple('BuckOut', 'link_trees targets_not_found')


class BuckException(Exception):
    pass


def presumed_target_root(target):
    target = target.lstrip('/')
    target = target.replace('/...', '')
    target = target.split(':')[0]
    return target


def _find_link_trees(targets_map):
    targets = list(targets_map.keys())
    targets_not_found = []
    link_trees = []
    for target in targets:
        target_path = target
        if target_path.startswith('//'):
            target_path = target_path[2:]
        target_path = target_path.replace(':', '/')

        discovered_link_trees = glob.glob(
            os.path.join('buck-out/gen/', target_path + '#*link-tree'))
        built = targets_map[target] and \
            len(glob.glob(targets_map[target])) > 0
        if (not target_path.endswith(('lib', 'library')) and
                len(discovered_link_trees) == 0 and
                not built):
            targets_not_found.append(target)
        link_trees.extend(
            [tree for tree in discovered_link_trees
                if not tree.endswith((
                    '-vs_debugger#link-tree',
                    '-interp#link-tree',
                    '-ipython#link-tree'))])
    return BuckOut(link_trees, targets_not_found)


def _normalize(target):
    LOG.info('Normalizing target `%s`', target)
    try:
        targets_to_destinations = subprocess.check_output(
            ['buck', 'targets', target, '--show-output'],
            stderr=subprocess.DEVNULL).decode().strip().split('\n')
        return targets_to_destinations
    except subprocess.CalledProcessError:
        raise BuckException(
            'Could not normalize target `{}`.\n   '.format(target),
            'Check the target path or run `buck clean`.')


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
    LOG.log(log.PROMPT, prompt + ' [Y/n] ')
    choice = input().strip().lower()
    return choice in ['', 'y', 'ye', 'yes']


def generate_link_trees(arguments, original_targets):
    buck_out = _find_link_trees({target: None for target in original_targets})
    link_trees = buck_out.link_trees

    if len(buck_out.targets_not_found) > 0:
        LOG.warning(
            'Passing in normalized buck targets will reduce runtime.\n   '
            'You can set up a .pyre_configuration file to reduce overhead.')

    for target in buck_out.targets_not_found:
        targets_to_destinations = _normalize(target)
        targets_map = {}

        for target in targets_to_destinations:
            pair = target.split(' ')
            if len(pair) > 1:
                targets_map[pair[0]] = pair[1]

        if arguments.build:
            _build_targets(target, list(targets_map.keys()))

        buck_out = _find_link_trees(targets_map)
        if len(buck_out.link_trees) == 0 and arguments.build:
            raise BuckException(
                'Could not find link trees for `{}`.\n   '
                'See `{} --help` for more '
                'information.'.format(target, sys.argv[0]))
        elif len(buck_out.link_trees) == 0:
            LOG.error(
                'Could not find link trees for `%s`.\n   '
                'The target might not be built.',
                target)
            if _get_yes_no_input("Build target?"):
                arguments.build = True
                return generate_link_trees(arguments, original_targets)
            raise BuckException(
                'Could not find link trees for `{}`.\n   '
                'See `{} --help` for more '
                'information.'.format(target, sys.argv[0]))
        elif len(buck_out.targets_not_found) > 0 and not arguments.build:
            LOG.error(
                'Could not find link trees for all targets in `%s`.\n   '
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

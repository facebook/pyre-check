# Copyright 2004-present Facebook.  All rights reserved.

import glob
import logging
import os
import subprocess
import sys
from collections import namedtuple
from . import log

LOG = logging.getLogger(__name__)


BuckOut = namedtuple('BuckOut', 'source_directories targets_not_found')


class BuckException(Exception):
    pass


def presumed_target_root(target):
    target = target.lstrip('/')
    target = target.replace('/...', '')
    target = target.split(':')[0]
    return target


def _find_source_directories(targets_map):
    targets = list(targets_map.keys())
    targets_not_found = []
    source_directories = []
    for target in targets:
        target_path = target
        if target_path.startswith('//'):
            target_path = target_path[2:]
        target_path = target_path.replace(':', '/')

        discovered_source_directories = glob.glob(
            os.path.join('buck-out/gen/', target_path + '#*link-tree'))
        target_destination = targets_map[target]
        built = (target_destination is not None and (
            target_destination == '' or
            len(glob.glob(target_destination)) > 0))
        if not built and len(discovered_source_directories) == 0:
            targets_not_found.append(target)
        source_directories.extend(
            [tree for tree in discovered_source_directories
                if not tree.endswith((
                    '-vs_debugger#link-tree',
                    '-interp#link-tree',
                    '-ipython#link-tree'))])
    return BuckOut(source_directories, targets_not_found)


def _normalize(target):
    LOG.info('Normalizing target `%s`', target)
    try:
        targets_to_destinations = subprocess.check_output(
            ['buck', 'targets', target, '--show-output'],
            stderr=subprocess.DEVNULL).decode().strip().split('\n')
        return targets_to_destinations
    except subprocess.CalledProcessError:
        raise BuckException(
            'Could not normalize target `{}`.\n   '
            'Check the target path or run `buck clean`.'.format(target))


def _build_targets(full_target_map) -> None:
    for original_target_name, targets_map in full_target_map.items():
        normalized_targets = targets_map.keys()
        LOG.info('Building `%s`', original_target_name)
        command = ['buck', 'build']
        command.extend(normalized_targets)
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


def generate_source_directories(original_targets, build):
    buck_out = _find_source_directories(
        {target: None for target in original_targets})
    source_directories = buck_out.source_directories

    full_targets_map = {}
    for original_target in buck_out.targets_not_found:
        targets_to_destinations = _normalize(original_target)
        normalized_targets_map = {}
        for target_destination_pair in targets_to_destinations:
            pair = target_destination_pair.split(' ')
            if len(pair) > 1:
                normalized_targets_map[pair[0]] = pair[1]
            else:
                normalized_targets_map[pair[0]] = ''
        full_targets_map[original_target] = normalized_targets_map

    if build:
        _build_targets(full_targets_map)

    unbuilt_targets = []
    for target_name, normalized_targets_map in full_targets_map.items():
        buck_out = _find_source_directories(normalized_targets_map)
        # Add anything that is unbuilt or only partially built
        if len(buck_out.targets_not_found) > 0:
            unbuilt_targets.append(target_name)
        source_directories.extend(buck_out.source_directories)

    if len(unbuilt_targets) > 0:
        if build:
            raise BuckException(
                'Could not find link trees for:\n    `{}`.\n   '
                'See `{} --help` for more information.'.format(
                    '    \n'.join(unbuilt_targets),
                    sys.argv[0]))
        else:
            LOG.error(
                'Could not find link trees for:\n    `%s`.\n   '
                'These targets might be unbuilt or only partially built.',
                '    \n'.join(unbuilt_targets))
            if _get_yes_no_input("Build target?"):
                return generate_source_directories(
                    original_targets,
                    build=True)
            raise BuckException(
                'Could not find link trees for:\n    `{}`.\n   '
                'See `{} --help` for more information.'.format(
                    '    \n'.join(unbuilt_targets),
                    sys.argv[0]))
    return source_directories

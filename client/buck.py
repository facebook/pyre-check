# Copyright 2004-present Facebook.  All rights reserved.

import glob
import logging
import os
import subprocess
import sys
from collections import namedtuple
from typing import List

from . import log


LOG = logging.getLogger(__name__)


BuckOut = namedtuple("BuckOut", "source_directories targets_not_found")


class BuckException(Exception):
    pass


def presumed_target_root(target):
    target = target.lstrip("/")
    target = target.replace("/...", "")
    target = target.split(":")[0]
    return target


def _find_source_directories(targets_map):
    targets = list(targets_map.keys())
    targets_not_found = []
    source_directories = []
    for target in targets:
        target_path = target
        if target_path.startswith("//"):
            target_path = target_path[2:]
        target_path = target_path.replace(":", "/")

        discovered_source_directories = glob.glob(
            os.path.join("buck-out/gen/", target_path + "#*link-tree")
        )
        target_destination = targets_map[target]
        built = target_destination is not None and (
            target_destination == "" or len(glob.glob(target_destination)) > 0
        )
        if not built and len(discovered_source_directories) == 0:
            targets_not_found.append(target)
        source_directories.extend(
            [
                tree
                for tree in discovered_source_directories
                if not tree.endswith(
                    (
                        "-vs_debugger#link-tree",
                        "-interp#link-tree",
                        "-ipython#link-tree",
                    )
                )
            ]
        )
    return BuckOut(source_directories, targets_not_found)


def _normalize(targets: List[str]) -> List[str]:
    LOG.info(
        "Normalizing target%s `%s`",
        "s:" if len(targets) > 1 else "",
        "`, `".join(targets),
    )
    try:
        command = ["buck", "targets"] + targets
        command.append("--show-output")
        targets_to_destinations = (
            subprocess.check_output(command, stderr=subprocess.DEVNULL, timeout=200)
            .decode()
            .strip()
            .split("\n")
        )
        return targets_to_destinations
    except subprocess.TimeoutExpired:
        raise BuckException(
            "Seems like `{}` is hanging.\n   "
            "Try running `buck clean` before trying again.".format(
                " ".join(command[:-1])
            )
        )
    except subprocess.CalledProcessError:
        raise BuckException(
            "Could not normalize targets. Check the paths or run `buck clean`."
        )


def _build_targets(targets: List[str]) -> None:
    LOG.info(
        "Building target%s `%s`", "s:" if len(targets) > 1 else "", "`, `".join(targets)
    )
    command = ["buck", "build"] + targets
    try:
        subprocess.check_output(command, stderr=subprocess.DEVNULL)
        LOG.warning("Finished building targets.")
    except subprocess.CalledProcessError:
        raise BuckException(
            "Could not build targets. Check the paths or run `buck clean`."
        )


def generate_source_directories(original_targets, build, prompt=True):
    buck_out = _find_source_directories({target: None for target in original_targets})
    source_directories = buck_out.source_directories

    full_targets_map = {}
    if buck_out.targets_not_found:
        targets_to_destinations = _normalize(buck_out.targets_not_found)

        for original_target in buck_out.targets_not_found:
            normalized_targets_map = {}
            for target_destination_pair in targets_to_destinations:
                pair = target_destination_pair.split(" ")
                if presumed_target_root(pair[0]).startswith(
                    presumed_target_root(original_target)
                ):
                    if len(pair) > 1:
                        normalized_targets_map[pair[0]] = pair[1]
                    else:
                        normalized_targets_map[pair[0]] = ""
            full_targets_map[original_target] = normalized_targets_map

    if build and full_targets_map:
        _build_targets(list(full_targets_map.keys()))

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
                "Could not find link trees for:\n    `{}`.\n   "
                "See `{} --help` for more information.".format(
                    "    \n".join(unbuilt_targets), sys.argv[0]
                )
            )
        else:
            LOG.error(
                "Could not find link trees for:\n    `%s`.\n   "
                "These targets might be unbuilt or only partially built.",
                "    \n".join(unbuilt_targets),
            )
            if not prompt or log.get_yes_no_input("Build target?"):
                return generate_source_directories(
                    original_targets, build=True, prompt=False
                )
            raise BuckException(
                "Could not find link trees for:\n    `{}`.\n   "
                "See `{} --help` for more information.".format(
                    "    \n".join(unbuilt_targets), sys.argv[0]
                )
            )
    return source_directories

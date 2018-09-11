# Copyright 2004-present Facebook.  All rights reserved.

import glob
import json
import logging
import os
import subprocess
import sys
from collections import namedtuple
from typing import Dict, List  # noqa

from . import log


LOG = logging.getLogger(__name__)
CACHE_PATH = ".pyre/buckcache.json"

BuckOut = namedtuple("BuckOut", "analysis_directories targets_not_found")


class BuckException(Exception):
    pass


def presumed_target_root(target):
    target = target.lstrip("/")
    target = target.replace("/...", "")
    target = target.split(":")[0]
    return target


def _find_analysis_directories(targets_map) -> BuckOut:
    targets = list(targets_map.keys())
    targets_not_found = []
    analysis_directories = []
    for target in targets:
        target_path = target
        if target_path.startswith("//"):
            target_path = target_path[2:]
        target_path = target_path.replace(":", "/")

        discovered_analysis_directories = glob.glob(
            os.path.join("buck-out/gen/", target_path + "#*link-tree")
        )
        target_destination = targets_map[target]
        built = target_destination is not None and (
            target_destination == "" or len(glob.glob(target_destination)) > 0
        )
        if not built and len(discovered_analysis_directories) == 0:
            targets_not_found.append(target)
        analysis_directories.extend(
            [
                tree
                for tree in discovered_analysis_directories
                if not tree.endswith(
                    (
                        "-vs_debugger#link-tree",
                        "-interp#link-tree",
                        "-ipython#link-tree",
                    )
                )
            ]
        )
    return BuckOut(analysis_directories, targets_not_found)


def _normalize(targets: List[str]) -> List[str]:
    LOG.info(
        "Normalizing target%s `%s`",
        "s:" if len(targets) > 1 else "",
        "`, `".join(targets),
    )
    try:
        command = (
            ["buck", "targets", "--show-output"]
            + targets
            + ["--type", "python_binary", "python_test"]
        )
        targets_to_destinations = (
            subprocess.check_output(command, stderr=subprocess.PIPE, timeout=200)
            .decode()
            .strip()
            .split("\n")
        )
        if len(targets_to_destinations) == 0:
            LOG.warning(
                "Provided TARGETS files do not contain any binary or unittest targets."
            )
            return []
        else:
            LOG.info(
                "Found %d buck target%s.",
                len(targets_to_destinations),
                "s" if len(targets_to_destinations) > 1 else "",
            )
        return targets_to_destinations
    except subprocess.TimeoutExpired:
        raise BuckException(
            "Seems like `{}` is hanging.\n   "
            "Try running `buck clean` before trying again.".format(
                # pyre-fixme: command not always defined
                " ".join(command[:-1])
            )
        )
    except subprocess.CalledProcessError as error:
        LOG.error("Buck returned error: %s" % error.stderr.decode().strip())
        raise BuckException(
            "Could not normalize targets. Check the paths or run `buck clean`."
        )


def _build_targets(targets: List[str]) -> None:
    LOG.info(
        "Building target%s `%s`", "s:" if len(targets) > 1 else "", "`, `".join(targets)
    )
    command = ["buck", "build"] + targets
    try:
        subprocess.check_output(command, stderr=subprocess.PIPE)
        LOG.warning("Finished building targets.")
    except subprocess.CalledProcessError as error:
        # The output can be overwhelming, hence print only the last 20 lines.
        lines = error.stderr.decode().splitlines()
        LOG.error("Buck returned error: %s" % "\n".join(lines[-20:]))
        raise BuckException(
            "Could not build targets. Check the paths or run `buck clean`."
        )


def generate_analysis_directories(original_targets, build, prompt: bool = True):
    buck_out = _find_analysis_directories({target: None for target in original_targets})
    analysis_directories = buck_out.analysis_directories

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
        buck_out = _find_analysis_directories(normalized_targets_map)
        # Add anything that is unbuilt or only partially built
        if len(buck_out.targets_not_found) > 0:
            unbuilt_targets.append(target_name)
        analysis_directories.extend(buck_out.analysis_directories)

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
                return generate_analysis_directories(
                    original_targets, build=True, prompt=False
                )
            raise BuckException(
                "Could not find link trees for:\n    `{}`.\n   "
                "See `{} --help` for more information.".format(
                    "    \n".join(unbuilt_targets), sys.argv[0]
                )
            )
    return analysis_directories
